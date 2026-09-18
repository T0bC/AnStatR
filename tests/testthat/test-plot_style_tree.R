box::use(
  shiny[reactive, testServer],
  testthat[
    describe,
    expect_equal,
    expect_false,
    expect_true,
    it,
  ],
)

box::use(
  app/view/plotting/style,
)

# =============================================================================
# Colors & Order tree: nested sortable lists
#
# Penguins-shaped fixture: Gentoo occurs only on Biscoe and Chinstrap only on
# Dream, so four of the nine SPECIES x ISLAND combinations carry no data.
# =============================================================================

fixture <- data.frame(
  SPECIES = c(rep("Adelie", 6), rep("Gentoo", 3), rep("Chinstrap", 3)),
  ISLAND = c(
    rep(c("Biscoe", "Dream", "Torgersen"), 2),
    rep("Biscoe", 3),
    rep("Dream", 3)
  ),
  SEX = rep(c("f", "m"), 6),
  Val = 1:12,
  stringsAsFactors = FALSE
)

# The module returns its reactives; capture them so tests can read them
capture_env <- new.env(parent = emptyenv())
style_server <- function(input, output, session) {
  result <- style$tab_server(
    input, output, session, reactive(fixture), reactive(1)
  )
  assign("reactives", result, envir = capture_env)
  result
}
captured <- function() capture_env$reactives

tree_html <- function(output) {
  as.character(output$colorOrderTree$html)
}

# Control input ids of one kind ("color_" / "shape_"), in render order
control_ids <- function(html, prefix) {
  ids <- regmatches(
    html, gregexpr(paste0('id="[^"]*', prefix, '[^"-]*"'), html)
  )[[1]]
  gsub('^id="|"$', "", ids)
}

# All DOM ids, excluding sortable's data-rank-id attributes
dom_ids <- function(html) {
  unlist(regmatches(
    html,
    gregexpr('(?<!data-rank-)id="[^"]*"', html, perl = TRUE)
  ))
}

# =============================================================================
# Browser model
#
# Every rendered sortable list shows factor_order()[[col]], and Shiny only
# forwards a value that differs from the one that list last sent.  Replaying
# that loop turns "the UI flickers" into a terminating / non-terminating
# check: a drag the server refuses to adopt gets pushed back by the next
# render, which is exactly the flicker.
# =============================================================================

# Column an order input belongs to: order_ISLAND__Adelie -> ISLAND
list_column <- function(list_id) {
  sub("^order_", "", sub("__.*$", "", list_id))
}

# Seed the client with the order every list shows on first render
new_client <- function(session, order_of, list_ids) {
  fo <- order_of()
  client <- lapply(list_ids, function(id) as.character(fo[[list_column(id)]]))
  names(client) <- list_ids
  do.call(session$setInputs, client)
  client
}

# Drag one list, then let renders and echoes run until nothing changes.
# `rounds` is NA when the loop never settles, i.e. the UI flickers.
drag_and_settle <- function(session, order_of, client, list_id, new_order,
                            max_rounds = 12) {
  client[[list_id]] <- new_order
  args <- list()
  args[[list_id]] <- new_order
  do.call(session$setInputs, args)

  for (round in seq_len(max_rounds)) {
    fo <- order_of()
    updates <- list()
    for (id in names(client)) {
      rendered <- as.character(fo[[list_column(id)]])
      if (!identical(client[[id]], rendered)) {
        updates[[id]] <- rendered
      }
    }
    if (length(updates) == 0) {
      return(list(client = client, rounds = round))
    }
    client[names(updates)] <- updates
    do.call(session$setInputs, updates)
  }
  list(client = client, rounds = NA_integer_)
}

island_lists <- c(
  "order_ISLAND__Adelie",
  "order_ISLAND__Gentoo",
  "order_ISLAND__Chinstrap"
)
all_lists <- c("order_SPECIES", island_lists)

describe("nested Colors & Order tree", {
  it("gives every branch's sortable list its own DOM id", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      ids <- dom_ids(tree_html(output))
      expect_false(any(duplicated(ids)))
      expect_equal(
        sum(grepl("mock-session-order_ISLAND__", ids, fixed = TRUE)),
        6 # one rank-list + one container per species
      )
    })
  })

  it("marks combinations without observations and drops their controls", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      html <- tree_html(output)
      # Gentoo x Dream, Gentoo x Torgersen, Chinstrap x Biscoe,
      # Chinstrap x Torgersen
      expect_equal(lengths(gregexpr('level-unavailable"', html)), 4L)
      # Only the five populated combinations get a shape input
      shape_ids <- regmatches(
        html, gregexpr('id="[^"]*shape_[^"-]*"', html)
      )[[1]]
      expect_equal(length(shape_ids), 5L)
    })
  })

  it("applies a drag in any branch to the whole column", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      expect_equal(
        captured()$factor_order()[["ISLAND"]],
        c("Biscoe", "Dream", "Torgersen")
      )

      dragged <- c("Dream", "Torgersen", "Biscoe")
      session$setInputs(order_ISLAND__Chinstrap = dragged)
      expect_equal(captured()$factor_order()[["ISLAND"]], dragged)

      # The browser echoes the new order back from every branch on re-render
      session$setInputs(
        order_ISLAND__Adelie = dragged,
        order_ISLAND__Gentoo = dragged,
        order_ISLAND__Chinstrap = dragged
      )
      expect_equal(captured()$factor_order()[["ISLAND"]], dragged)
    })
  })

  it("keeps outer and inner column orders independent", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(order_ISLAND__Adelie = c("Dream", "Biscoe", "Torgersen"))
      session$setInputs(order_SPECIES = c("Gentoo", "Adelie", "Chinstrap"))

      expect_equal(
        captured()$factor_order()[["SPECIES"]],
        c("Gentoo", "Adelie", "Chinstrap")
      )
      expect_equal(
        captured()$factor_order()[["ISLAND"]],
        c("Dream", "Biscoe", "Torgersen")
      )
    })
  })

  it("moves the controls up when Color by is a subset of the X-axis", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(pointColor = "SPECIES")

      html <- tree_html(output)
      expect_equal(
        captured()$color_groups(),
        c("Adelie", "Gentoo", "Chinstrap")
      )
      # One control pair per species, keyed by the colour group itself
      expect_equal(
        control_ids(html, "color_"),
        c(
          "mock-session-color_Adelie",
          "mock-session-color_Gentoo",
          "mock-session-color_Chinstrap"
        )
      )

      # ... and edits there actually reach the maps handed to the plot
      session$setInputs(color_Adelie = "#123456")
      expect_equal(captured()$color_map()[["Adelie"]], "#123456")

      # "Shape by" is still empty, so shapes stay at the finest grouping
      expect_equal(
        control_ids(html, "shape_"),
        c(
          "mock-session-shape_Adelie_Biscoe",
          "mock-session-shape_Adelie_Dream",
          "mock-session-shape_Adelie_Torgersen",
          "mock-session-shape_Gentoo_Biscoe",
          "mock-session-shape_Chinstrap_Dream"
        )
      )
      session$setInputs(shape_Adelie_Biscoe = "22")
      expect_equal(captured()$shape_map()[["Adelie.Biscoe"]], 22L)
    })
  })

  it("falls back to a flat list when Color by does not match the nesting", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(pointColor = "ISLAND")

      html <- tree_html(output)
      expect_false(any(duplicated(dom_ids(html))))
      expect_equal(
        control_ids(html, "color_"),
        c(
          "mock-session-color_Biscoe",
          "mock-session-color_Dream",
          "mock-session-color_Torgersen"
        )
      )

      session$setInputs(color_Dream = "#abcdef")
      expect_equal(captured()$color_map()[["Dream"]], "#abcdef")
    })
  })

  it("still reorders a single X-axis column", {
    testServer(style_server, {
      session$setInputs(xAxis = "SPECIES", plotType = "scatter")
      session$setInputs(order_SPECIES = c("Chinstrap", "Adelie", "Gentoo"))
      expect_equal(
        captured()$factor_order()[["SPECIES"]],
        c("Chinstrap", "Adelie", "Gentoo")
      )
      expect_true(length(captured()$color_groups()) == 3)
    })
  })
})

describe("dragging settles without flicker", {
  reversed <- c("Torgersen", "Dream", "Biscoe")
  shuffled <- c("Dream", "Biscoe", "Torgersen")

  it("adopts a drag in any branch, complete or incomplete", {
    for (branch in island_lists) {
      testServer(style_server, {
        session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
        order_of <- function() captured()$factor_order()
        client <- new_client(session, order_of, all_lists)

        settled <- drag_and_settle(
          session, order_of, client, branch, reversed
        )

        expect_false(is.na(settled$rounds))
        expect_equal(order_of()[["ISLAND"]], reversed)
        # Every branch ends up showing the same order
        for (id in island_lists) {
          expect_equal(settled$client[[id]], reversed)
        }
      })
    }
  })

  it("survives a complete branch, then an incomplete one, then another", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      order_of <- function() captured()$factor_order()
      client <- new_client(session, order_of, all_lists)

      # Adelie has all three islands, Gentoo only Biscoe, Chinstrap only Dream
      step1 <- drag_and_settle(
        session, order_of, client, "order_ISLAND__Adelie", shuffled
      )
      expect_false(is.na(step1$rounds))
      expect_equal(order_of()[["ISLAND"]], shuffled)

      step2 <- drag_and_settle(
        session, order_of, step1$client, "order_ISLAND__Gentoo", reversed
      )
      expect_false(is.na(step2$rounds))
      expect_equal(order_of()[["ISLAND"]], reversed)

      step3 <- drag_and_settle(
        session, order_of, step2$client, "order_ISLAND__Chinstrap", shuffled
      )
      expect_false(is.na(step3$rounds))
      expect_equal(order_of()[["ISLAND"]], shuffled)

      for (id in island_lists) {
        expect_equal(step3$client[[id]], shuffled)
      }
    })
  })

  it("keeps settling when outer and inner drags are interleaved", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      order_of <- function() captured()$factor_order()
      client <- new_client(session, order_of, all_lists)

      species <- c("Gentoo", "Chinstrap", "Adelie")
      step1 <- drag_and_settle(
        session, order_of, client, "order_ISLAND__Gentoo", reversed
      )
      step2 <- drag_and_settle(
        session, order_of, step1$client, "order_SPECIES", species
      )
      step3 <- drag_and_settle(
        session, order_of, step2$client, "order_ISLAND__Chinstrap", shuffled
      )

      expect_false(is.na(step1$rounds))
      expect_false(is.na(step2$rounds))
      expect_false(is.na(step3$rounds))
      expect_equal(order_of()[["SPECIES"]], species)
      expect_equal(order_of()[["ISLAND"]], shuffled)
    })
  })

  it("settles the same way when Color by is a single column", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(pointColor = "SPECIES")
      order_of <- function() captured()$factor_order()
      client <- new_client(session, order_of, all_lists)

      settled <- drag_and_settle(
        session, order_of, client, "order_ISLAND__Gentoo", reversed
      )

      expect_false(is.na(settled$rounds))
      expect_equal(order_of()[["ISLAND"]], reversed)
    })
  })

  it("settles for a single X-axis column", {
    testServer(style_server, {
      session$setInputs(xAxis = "SPECIES", plotType = "scatter")
      order_of <- function() captured()$factor_order()
      client <- new_client(session, order_of, "order_SPECIES")

      species <- c("Chinstrap", "Gentoo", "Adelie")
      settled <- drag_and_settle(
        session, order_of, client, "order_SPECIES", species
      )

      expect_false(is.na(settled$rounds))
      expect_equal(order_of()[["SPECIES"]], species)
    })
  })
})

# =============================================================================
# Color by / Shape by are independent groupings
# =============================================================================

# Group keys the colour and shape controls are rendered for
control_groups <- function(html, prefix) {
  gsub(
    paste0("^mock-session-", prefix), "",
    control_ids(html, prefix)
  )
}

describe("Color by and Shape by place controls independently", {
  it("puts both at the leaf when neither is set", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      html <- tree_html(output)
      leaves <- c(
        "Adelie_Biscoe", "Adelie_Dream", "Adelie_Torgersen",
        "Gentoo_Biscoe", "Chinstrap_Dream"
      )
      expect_equal(control_groups(html, "color_"), leaves)
      expect_equal(control_groups(html, "shape_"), leaves)
    })
  })

  it("keeps shapes at the leaf when only Color by is set", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(pointColor = "SPECIES")
      html <- tree_html(output)
      expect_equal(
        control_groups(html, "color_"),
        c("Adelie", "Gentoo", "Chinstrap")
      )
      expect_equal(length(control_groups(html, "shape_")), 5L)
    })
  })

  it("keeps colours at the leaf when only Shape by is set", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(pointShape = "SPECIES")
      html <- tree_html(output)
      expect_equal(
        control_groups(html, "shape_"),
        c("Adelie", "Gentoo", "Chinstrap")
      )
      expect_equal(length(control_groups(html, "color_")), 5L)
    })
  })

  it("behaves like neither being set when both name every X column", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(
        pointColor = c("SPECIES", "ISLAND"),
        pointShape = c("SPECIES", "ISLAND")
      )
      html <- tree_html(output)
      leaves <- c(
        "Adelie_Biscoe", "Adelie_Dream", "Adelie_Torgersen",
        "Gentoo_Biscoe", "Chinstrap_Dream"
      )
      expect_equal(control_groups(html, "color_"), leaves)
      expect_equal(control_groups(html, "shape_"), leaves)
    })
  })

  it("gives Shape by a flat list when it is not an X-axis column", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(pointColor = "SPECIES", pointShape = "SEX")
      html <- tree_html(output)

      expect_false(any(duplicated(dom_ids(html))))
      expect_equal(control_groups(html, "shape_"), c("f", "m"))
      expect_true(grepl("Shapes: SEX", html, fixed = TRUE))

      session$setInputs(shape_f = "24")
      expect_equal(captured()$shape_map()[["f"]], 24L)
    })
  })

  it("never disables the shape dropdowns", {
    testServer(style_server, {
      session$setInputs(xAxis = c("SPECIES", "ISLAND"), plotType = "scatter")
      session$setInputs(pointShape = "SPECIES")
      expect_false(grepl("shape-disabled", tree_html(output), fixed = TRUE))
    })
  })
})
