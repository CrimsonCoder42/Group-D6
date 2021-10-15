#Functions to create neatly-spaced rows of buttons
buttonRow1 <- function(inputIds, labels, btnStyle) {
  fluidRow(
    column(2,offset = 4,
           actionButton(inputId = inputIds[1], label =  labels[1],style=btnStyle)),
  ) #row
}
buttonRow2 <- function(inputIds, labels, btnStyle) {
  fluidRow(
    column(2,offset = 2,
           actionButton(inputId = inputIds[1], label =  labels[1],style=btnStyle)),
    column(2,offset = 2,
           actionButton(inputId = inputIds[2], label =  labels[2],style=btnStyle))
  ) #row
}
buttonRow3 <- function(inputIds, labels, btnStyle) {
  fluidRow(
    column(1,offset = 1,
           actionButton(inputId = inputIds[1], label =  labels[1],style=btnStyle)),
    column(1,offset = 2,
           actionButton(inputId = inputIds[2], label =  labels[2],style=btnStyle)),
    column(1,offset = 2,
           actionButton(inputId = inputIds[3], label =  labels[3],style=btnStyle))
  ) #row
}
buttonRow4 <- function(inputIds, labels, btnStyle) {
  fluidRow(
    column(2,
           actionButton(inputId = inputIds[1], label =  labels[1],style=btnStyle)),
    column(2,offset = 1,
           actionButton(inputId = inputIds[2], label =  labels[2],style=btnStyle)),
    column(2,offset = 1,
           actionButton(inputId = inputIds[3], label =  labels[3],style=btnStyle)),
    column(2,offset = 1,
           actionButton(inputId = inputIds[4], label =  labels[4],style=btnStyle))
  ) #row
}
buttonRow5 <- function(inputIds, labels, btnStyle) {
  fluidRow(
    column(1,offset = 1,
           actionButton(inputId = inputIds[1], label =  labels[1], style=btnStyle)),
    column(1,offset = 1,
           actionButton(inputId = inputIds[2], label =  labels[2],style=btnStyle)),
    column(1,offset = 1,
           actionButton(inputId = inputIds[3], label =  labels[3],style=btnStyle)),
    column(1,offset = 1,
           actionButton(inputId = inputIds[4], label =  labels[4],style=btnStyle)),
    column(1,offset = 1,
           actionButton(inputId = inputIds[5], label =  labels[5],style=btnStyle))
  ) #row
}

#Functions to create neatly-spaced rows of placeholders for buttons
controlRow1 <- function(ctrlId) {
  fluidRow(
    column(2,offset = 4,
           uiOutput(outputId = ctrlId)
    )
  ) #row
}
controlRow2 <- function(ctrlIds) {
  fluidRow(
    column(2,offset = 2,
           uiOutput(outputId = ctrlIds[1])
    ),
    column(2,offset = 2,
           uiOutput(outputId = ctrlIds[2])
    )
  ) #row
}
controlRow3 <- function(ctrlIds) {
  fluidRow(
    column(1,offset = 1,
           uiOutput(outputId = ctrlIds[1])
    ),
    column(1,offset = 2,
           uiOutput(outputId = ctrlIds[2])
    ),
    column(1,offset = 2,
           uiOutput(outputId = ctrlIds[3])
    ),
  ) #row
}
controlRow4 <- function(ctrlIds) {
  fluidRow(
    column(1,offset = 1,
           uiOutput(outputId = ctrlIds[1])
    ),
    column(1,offset = 1,
           uiOutput(outputId = ctrlIds[2])
    ),
    column(1,offset = 1,
           uiOutput(outputId = ctrlIds[3])
    ),
    column(1,offset = 1,
           uiOutput(outputId = ctrlIds[4])
    )
  ) #row
}

controlRow5 <- function(ctrlIds, ctrl_labels) {
  fluidRow(
    column(1,offset = 1,
           h4(ctrl_labels[1]),
           uiOutput(outputId = ctrlIds[1])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[2]),
           uiOutput(outputId = ctrlIds[2])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[3]),
           uiOutput(outputId = ctrlIds[3])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[4]),
           uiOutput(outputId = ctrlIds[4])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[5]),
           uiOutput(outputId = ctrlIds[5])
    )
  ) #row
}

controlRow6 <- function(ctrlIds, ctrl_labels) {
  fluidRow(
    column(1,offset = 1,
           h4(ctrl_labels[1]),
           uiOutput(outputId = ctrlIds[1])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[2]),
           uiOutput(outputId = ctrlIds[2])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[3]),
           uiOutput(outputId = ctrlIds[3])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[4]),
           uiOutput(outputId = ctrlIds[4])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[5]),
           uiOutput(outputId = ctrlIds[5])
    ),
    column(1,offset = 1,
           h4(ctrl_labels[6]),
           uiOutput(outputId = ctrlIds[6])
    )
  ) #row
}
