# Loading packages
library(latex2exp)
library(mvtnorm)
library(scales)
# Pre-settings
fig.gen <- TRUE
color <- c("firebrick3", "deepskyblue3", "darkolivegreen4")
length.axis <- 4
#### Figure 1: Brownian bridge (Accuracy) ####
# Reading data
load(file = "data/FigBB.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  pdf(paste0("img/BB.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}
# Main plot
y.min.max <- range(bnd.bb, 0.8399 * max(c) * sqrt(t.final))
matplot(t.line, bnd.bb, type = "l", lty = 1, ylim = c(y.min.max[1], y.min.max[2]),
        xlab = "", ylab = "", col = color, lwd = 3)
matlines(t.line, t(0.8399 * c * matrix(rep(sqrt(t.final - t.line), each = 3), nrow = 3)),
         lty = c(2, 3, 4), col = "black", lwd = 3)
# Main plot's legend
if (fig.gen) {
  legend(x = 0.76, y = 1.88, bty = "n", title = "",
         legend = c(TeX(paste0("$\\tilde{\\nu} = ", "$", c[1], "$")),
                    TeX(paste("$\\tilde{\\nu} = ", "$", c[2], "$")),
                    TeX(paste("$\\tilde{\\nu} = ", "$", c[3], "$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.83, y = 1.92, bty = "n", title = "",
         legend = c(TeX(paste0("$\\tilde{\\nu} = ", "$", c[1], "$")),
                    TeX(paste("$\\tilde{\\nu} = ", "$", c[2], "$")),
                    TeX(paste("$\\tilde{\\nu} = ", "$", c[3], "$"))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}
# Errors' plots
err.min.max <- range(log.err.bb)
# 1st error plot
plot(1:length(log.err.bb[, 1]), log.err.bb[, 1], pch = 8, col = color[1], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(log.err.bb[, 1]), l = length.axis)))
lines(1:length(log.err.bb[, 1]), log.err.bb[, 1], lty = 1, col = color[1])
# 2nd error plot
plot(1:length(log.err.bb[, 2]), log.err.bb[, 2], pch = 8, col = color[2], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(log.err.bb[, 2]), l = length.axis)))
lines(1:length(log.err.bb[, 2]), log.err.bb[, 2], lty = 1, col = color[2])
# 3rd plot
plot(1:length(log.err.bb[, 3]), log.err.bb[, 3], pch = 8, col = color[3], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(log.err.bb[, 3]), l = length.axis)))
lines(1:length(log.err.bb[, 3]), log.err.bb[, 3], lty = 1, col = color[3])
# Saving pdf (if fig.gen = TRUE)
if (fig.gen) dev.off()
#### Figure 2: Partition size (Convergence) ####
# Reading data
load(file = "data/FigConvergence.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  pdf(paste0("img/convergence.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}
# Main plot
y.min.max <- range(bnd.conv, 0.8399 * c * sqrt(t.final))
# Plot 1
plot(t.line[[1]], bnd.conv[[1]], type = "l", lty = 1, ylim = c(y.min.max[1], y.min.max[2]),
     xlab = "", ylab = "", col = color[1], lwd = 3)
lines(t.line[[2]], bnd.conv[[2]], lty = 1, col = color[2], lwd = 3)
lines(t.line[[3]], bnd.conv[[3]], lty = 1, col = color[3], lwd = 3)
lines(t.line[[3]], 0.8399 * c * sqrt(t.final - t.line[[3]]), lty = 2, lwd = 3)
# Main plot's legend
if (fig.gen) {
  legend(x = 0.75, y = 0.94, bty = "n", title = "",
         legend = c(TeX(paste0("$N = ", N[1], "$")),
                    TeX(paste0("$N = ", N[2], "$")),
                    TeX(paste0("$N = ", N[3], "$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.82, y = 0.96, bty = "n", title = "",
         legend = c(TeX(paste0("$N = ", N[1], "$")),
                    TeX(paste0("$N = ", N[2], "$")),
                    TeX(paste0("$N = ", N[3], "$"))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}
# Errors' plots
err.min.max <- range(log.err.conv)
# 1st error plot
plot(1:length(log.err.conv[, 1]), log.err.conv[, 1], pch = 8, col = color[1], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(log.err.conv[, 1]), l = length.axis)))
lines(1:length(log.err.conv[, 1]), log.err.conv[, 1], lty = 1, col = color[1])
# 2nd error plot
plot(1:length(log.err.conv[, 2]), log.err.conv[, 2], pch = 8, col = color[2], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(log.err.conv[, 2]), l = length.axis)))
lines(1:length(log.err.conv[, 2]), log.err.conv[, 2], lty = 1, col = color[2])
# 3rd plot
plot(1:length(log.err.conv[, 3]), log.err.conv[, 3], pch = 8, col = color[3], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(log.err.conv[, 3]), l = length.axis)))
lines(1:length(log.err.conv[, 3]), log.err.conv[, 3], lty = 1, col = color[3])
# Saving pdf (if fig.gen = TRUE)
if (fig.gen) dev.off()
#### Figure 3: Partition effect ####
# Reading data
load(file = "data/FigPartition.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  pdf(paste0("img/partition_effect.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}
# Main plot
y.min.max <- range(bnd.equal$boundary, bnd.shrink$boundary, bnd.log$boundary, 0.8399 * c * sqrt(t.final))
plot(t.line.equal, bnd.equal$boundary, type = "l", lty = 1, ylim = c(y.min.max[1], y.min.max[2]),
     xlab = "", ylab = "", col = color[1], lwd = 3)
lines(t.line.shrink, bnd.shrink$boundary, lty = 1, lwd = 3, col = color[2])
lines(t.line.log, bnd.log$boundary, lty = 1, lwd = 3, col = color[3])
lines(seq(0, t.final, l = 500), 0.8399 * c * sqrt(seq(t.final, 0, l = 500)), lty = 3, lwd = 3)
# Main plot's legend
if (fig.gen) {
  legend(x = 0.75, y = 0.92, bty = "n", title = "",
         legend = c("Homogeneous", "Suden shrink", "Logarithmic"),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.75, y = 0.96, bty = "n", title = "",
         legend = c("Homogeneous", "Suden shrink", "Logarithmic"),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}
# Errors' plots
err.min.max <- log(range(bnd.equal$errors, bnd.shrink$errors, bnd.log$error))
# 1st error plot
plot(1:length(bnd.equal$errors), log(bnd.equal$errors), pch = 8, col = color[1],
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(bnd.equal$errors), l = length.axis)))
lines(1:length(bnd.equal$errors), log(bnd.equal$errors), lty = 1, col = color[1])
# 2nd error plot
plot(1:length(bnd.shrink$errors), log(bnd.shrink$errors), pch = 8, col = color[2],
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(bnd.shrink$errors), l = length.axis)))
lines(1:length(bnd.shrink$errors), log(bnd.shrink$errors), lty = 1, col = color[2])
# 3rd plot
plot(1:length(bnd.log$errors), log(bnd.log$errors), pch = 8, col = color[3],
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(bnd.log$errors), l = length.axis)))
lines(1:length(bnd.log$errors), log(bnd.log$errors), lty = 1, col = color[3])
# Saving pdf (if fig.gen = TRUE)
if (fig.gen) dev.off()
#### Figure 3.1: Partition error ####
# Reading data
load(file = "data/PartitionError.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  pdf(paste0("img/partition_error.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(1, nrow = 1, byrow = TRUE), heights = 1)
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
  layout(matrix(1, nrow = 1, byrow = TRUE), heights = 1)
}
# Main plot
x.axis <- N[-(1:6)]
y.min.max <- range(error.equal[-(1:6)], error.shrink[-(1:6)], error.log[-(1:6)])
plot(x.axis, error.equal[-(1:6)], type = "l", lty = 1, ylim = c(y.min.max[1], y.min.max[2]),
     xlab = TeX("Partition size $N$"), ylab = TeX("Relative $L_2$ error"), col = color[1], lwd = 2)
lines(x.axis, error.shrink[-(1:6)], lty = 1, lwd = 2, col = color[2])
lines(x.axis, error.log[-(1:6)], lty = 1, lwd = 2, col = color[3])
# Main plot's legend
if (fig.gen) {
  legend(x = 380, y = 0.0064, bty = "n", title = "",
         legend = c("Homogeneous", "Suden shrink", "Logarithmic"),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 385, y = 0.0064, bty = "n", title = "",
         legend = c("Homogeneous", "Suden shrink", "Logarithmic"),
         col = color, lty = 1, cex = 1, xpd = TRUE, lwd = 2)
}
# Saving pdf (if fig.gen = TRUE)
if (fig.gen) dev.off()
#### Figure 3 + 3.1: Partition effect (plus) ####
# Reading data
load(file = "data/FigPartition.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  pdf(paste0("img/partition_effect_plus.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}
# Main plot
y.min.max <- range(bnd.equal$boundary, bnd.shrink$boundary, bnd.log$boundary, 0.8399 * c * sqrt(t.final))
plot(t.line.equal, bnd.equal$boundary, type = "l", lty = 1, ylim = c(y.min.max[1], y.min.max[2]),
     xlab = "", ylab = "", col = color[1], lwd = 3)
lines(t.line.shrink, bnd.shrink$boundary, lty = 1, lwd = 3, col = color[2])
lines(t.line.log, bnd.log$boundary, lty = 1, lwd = 3, col = color[3])
lines(seq(0, t.final, l = 500), 0.8399 * c * sqrt(seq(t.final, 0, l = 500)), lty = 3, lwd = 3)
# Main plot's legend
if (fig.gen) {
  legend(x = 0.64, y = 0.94, bty = "n", title = "",
         legend = c("Homogeneous", "Suden shrink", "Logarithmic"),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.75, y = 0.96, bty = "n", title = "",
         legend = c("Homogeneous", "Suden shrink", "Logarithmic"),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}
  ## Loading data 2
  load(file = "data/PartitionError.RData")
  # Plot settings 2
  if (fig.gen) { # Settng for saving the PDF image
    par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
        mgp = c(2, 0.6, 0), tcl = -0.4, new = TRUE) # Setting for plotting in RStudio
    layout(matrix(c(2, 3, 4, 5, 9, 6, 7, 8, 1), nrow = 3, byrow = TRUE),
           widths = c(0.05, 0.77, 0.18), heights = c(0.1, 0.46, 0.34))
  } else {
    par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
        mgp = c(2, 0.6, 0), tcl = -0.4, new = TRUE) # Setting for plotting in RStudio
    layout(matrix(c(2, 3, 4, 5, 9, 6, 7, 8, 1), nrow = 3, byrow = TRUE),
          widths = c(0.035, 0.8, 0.165), heights = c(0.1, 0.45, 0.35))
  }
  # Main plot
  x.axis <- N[-(1:6)]
  y.min.max <- range(error.equal[-(1:6)], error.shrink[-(1:6)], error.log[-(1:6)])
  plot(x.axis, error.equal[-(1:6)], type = "l", lty = 1, ylim = c(y.min.max[1], y.min.max[2]),
       xlab = "", ylab = "", xaxt = "n", col = color[1], lwd = 2, bty="n")
  axis(side = 1, at = (0:5)*100, labels = (0:5)*100)
       # xlab = TeX("Partition size $N$"), ylab = TeX("Relative $L_2$ error"), col = color[1], lwd = 2)
  lines(x.axis, error.shrink[-(1:6)], lty = 1, lwd = 2, col = color[2])
  lines(x.axis, error.log[-(1:6)], lty = 1, lwd = 2, col = color[3])
# Reading data
load(file = "data/FigPartition.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8, new = TRUE) # Setting for printing in RStudio
  layout(matrix(c(1, 2, 3, 6, 5, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4, new = TRUE) # Setting for plotting in RStudio
  layout(matrix(c(1, 2, 3, 6, 5, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}
# Errors' plots
err.min.max <- log(range(bnd.equal$errors, bnd.shrink$errors, bnd.log$error))
# 1st error plot
plot(1:length(bnd.equal$errors), log(bnd.equal$errors), pch = 8, col = color[1],
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(bnd.equal$errors), l = length.axis)))
lines(1:length(bnd.equal$errors), log(bnd.equal$errors), lty = 1, col = color[1])
# 2nd error plot
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8, new = TRUE) # Setting for printing in RStudio
  layout(matrix(c(1, 2, 3, 5, 6, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4, new = TRUE) # Setting for plotting in RStudio
  layout(matrix(c(1, 2, 3, 5, 6, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}
plot(1:length(bnd.shrink$errors), log(bnd.shrink$errors), pch = 8, col = color[2],
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(bnd.shrink$errors), l = length.axis)))
lines(1:length(bnd.shrink$errors), log(bnd.shrink$errors), lty = 1, col = color[2])
# 3rd plot
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8, new = TRUE) # Setting for printing in RStudio
  layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE), heights = c(2, 1))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4, new = TRUE) # Setting for plotting in RStudio
  layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE), heights = c(2, 1))
}
plot(1:length(bnd.log$errors), log(bnd.log$errors), pch = 8, col = color[3],
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(bnd.log$errors), l = length.axis)))
lines(1:length(bnd.log$errors), log(bnd.log$errors), lty = 1, col = color[3])
# Saving pdf (if fig.gen = TRUE)
if (fig.gen) dev.off()
#### Figure 4: Changing the pulling level ####
# Reading data
load(file = "data/PullingLevel.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  pdf(paste0("img/pulling_level.pdf"), width = , height = 10)
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 3, 5, 
                  2, 4, 6,
                  7, 7, 7, 
                  8, 9, 10), nrow = 4, byrow = TRUE), heights = c(2, 2, 4, 2))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
  layout(matrix(c(1, 3, 5, 
                  2, 4, 6,
                  7, 7, 7, 
                  8, 9, 10), nrow = 4, byrow = TRUE), heights = c(2, 2, 4, 2))
}
# Coefficients plot
for (i in 1:ncol(f1.t)) {
  # slope graph
  f1.t_ <- 1 / f1.t[, i] # Inverse f1.t
  f1.bridge.t_ <- 1 / f1.bridge.t[, i] # Inverse f1.bridge.t
  slopes <- cbind(f1.t_, f1.bridge.t_)
  matplot(t.line, slopes, type = "l", lty = c(1, 2), lwd = 2, 
          col = scales::alpha(color[i], c(0.5, 1)), xlab = "", ylab = "")
  lines(t.line, rep(0, length(t.line)), lty = 3, lwd = 2, col = "black")
  # drift graph
  f2.t[, i] <- f2.t[, i] / max(abs(f2.t[, i])) # Normalized f2.t
  f2.bridge.t[, i] <- f2.bridge.t[, i] / max(abs(f2.bridge.t[, i])) # Normalized f2.bridge.t
  level <- cbind(f2.t[, i], f2.bridge.t[, i])
  matplot(t.line, level, type = "l", lty = c(1, 2), lwd = 2, ylim = c(min(level), max(level)),
          col = scales::alpha(color[i], c(0.5, 1)), xlab = "", ylab = "")
  lines(t.line, rep(x.final, length(t.line)), lty = 3, lwd = 2, col = "black")
}
# Main plot
matplot(t.line, bnd.level, type = "l", lty = 1, ylim = range(bnd.level),
        xlab = "", ylab = "", col = color, lwd = 3)
# Main plot's legend
if (fig.gen) {
  legend(x = 0.43, y = 1.63, bty = "n", title = "",
         legend = c(TeX(paste0("$\\tilde{\\kappa}(t) = ", b, " + ", b/2, " $\\sin(8\\pi t)$")),
                    TeX(paste0("$\\tilde{\\kappa}(t) = ", b/2, "(2\\Phi(50t - 25) - 1)$")),
                    TeX(paste0("$\\tilde{\\kappa}(t) = ", -b/2, "$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2, y.intersp = 1.3)
} else {
  legend(x = 0.57, y = 1.57, bty = "n", title = "",
         legend = c(TeX(paste0("$\\tilde{\\kappa}(t) = $", b, " + ", b/2, " $\\sin(8\\pi t)$")),
                    TeX(paste0("$\\tilde{\\kappa}(t) = $", b/2, "$(2\\Phi(50t - 25) - 1)$")),
                    TeX(paste0("$\\tilde{\\kappa}(t) = $", -b/2))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}
# Errors' plots
err.min.max <- range(log.err.level)
# 1st error plot
plot(1:length(log.err.level[[1]]), log.err.level[[1]], pch = 8, col = color[1], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.level[[1]]))
# axis(side = 1, at = floor(seq(1, length(log.err.level[[1]]), l = length.axis)))
lines(1:length(log.err.level[[1]]), log.err.level[[1]], lty = 1, col = color[1])
# 2nd error plot
plot(1:length(log.err.level[[2]]), log.err.level[[2]], pch = 8, col = color[2], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.level[[2]]))
# axis(side = 1, at = floor(seq(1, length(log.err.level[[2]]), l = length.axis)))
lines(1:length(log.err.level[[2]]), log.err.level[[2]], lty = 1, col = color[2])
# 3rd plot
plot(1:length(log.err.level[[3]]), log.err.level[[3]], pch = 8, col = color[3], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.level[[3]]))
# axis(side = 1, at = floor(seq(1, length(log.err.level[[3]]), l = length.axis)))
lines(1:length(log.err.level[[3]]), log.err.level[[3]], lty = 1, col = color[3])
# Saving pdf (if fig.gen = TRUE)
if (fig.gen) dev.off()
#### Figure 5: Changing the slope ####
# Reading data
load(file = "data/Slope.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  pdf(paste0("img/slope.pdf"), width = , height = 10)
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 3, 5, 
                  2, 4, 6,
                  7, 7, 7, 
                  8, 9, 10), nrow = 4, byrow = TRUE), heights = c(2, 2, 4, 2))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
  layout(matrix(c(1, 3, 5, 
                  2, 4, 6,
                  7, 7, 7, 
                  8, 9, 10), nrow = 4, byrow = TRUE), heights = c(2, 2, 4, 2))
}
# Coefficients plot
for (i in 1:ncol(f1.t)) {
  # slope graph
  f1.t_ <- 1 / f1.t[, i] # Inverse f1.t
  f1.bridge.t_ <- 1 / f1.bridge.t[, i] # Inverse f1.bridge.t
  slopes <- cbind(f1.t_, f1.bridge.t_)
  matplot(t.line, slopes, type = "l", lty = c(1, 2), lwd = c(2, 2), 
          col = scales::alpha(color[i], c(0.5, 1)), xlab = "", ylab = "")
  lines(t.line, rep(0, length(t.line)), lty = 3, lwd = 2, col = "black")
  # drift graph
  f2.t[, i] <- f2.t[, i] / max(abs(f2.t[, i])) # Normalized f2.t
  f2.bridge.t[, i] <- f2.bridge.t[, i] / max(abs(f2.bridge.t[, i])) # Normalized f2.bridge.t
  level <- cbind(f2.t[, i], f2.bridge.t[, i])
  matplot(t.line, level, type = "l", lty = c(1, 2), lwd = c(2, 2), ylim = c(min(level), max(level)),
          col = scales::alpha(color[i], c(0.5, 1)), xlab = "", ylab = "")
  lines(t.line, rep(x.final, length(t.line)), lty = 3, lwd = 2, col = "black")
}
# Main plot
y.min.max <- range(bnd.slope)
matplot(t.line, bnd.slope, type = "l", lty = 1, ylim = c(y.min.max[1], y.min.max[2] + 0.29),
        xlab = "", ylab = "", col = color, lwd = 3)
# Main plot's legend
if (fig.gen) {
  legend(x = 0.425, y = 1.14, bty = "n", title = "",
         legend = c(TeX(paste0("$\\tilde{\\theta}(t) =\\, e^{", a, "t}$")),
                    TeX(paste0("$\\tilde{\\theta}(t) =\\, 1 + ", 5*a, "\\sqrt{2\\pi}\\phi(20t - 10)$")),
                    TeX(paste0("$\\tilde{\\theta}(t) =\\, e^{", -a, "t}$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2, y.intersp = 1.3)
} else {
  legend(x = 0.53, y = 1.1, bty = "n", title = "",
         legend = c(TeX(paste0("$\\tilde{\\theta}(t) =\\, e^{", a, "t}$")),
                    TeX(paste0("$\\tilde{\\theta}(t) =\\, 1 + ", 5*a, "\\sqrt{2\\pi}\\phi(20t - 10)$")),
                    TeX(paste0("$\\tilde{\\theta}(t) =\\, e^{", -a, "t}$"))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}
# Errors' plots
err.min.max <- range(log.err.slope)
# 1st error plot
plot(1:length(log.err.slope[[1]]), log.err.slope[[1]], pch = 8, col = color[1], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.slope[[1]]))
# axis(side = 1, at = floor(seq(1, length(log.err.slope[[1]]), l = length.axis)))
lines(1:length(log.err.slope[[1]]), log.err.slope[[1]], lty = 1, col = color[1])
# 2nd error plot
plot(1:length(log.err.slope[[2]]), log.err.slope[[2]], pch = 8, col = color[2], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.slope[[2]]))
# axis(side = 1, at = floor(seq(1, length(log.err.slope[[2]]), l = length.axis)))
lines(1:length(log.err.slope[[2]]), log.err.slope[[2]], lty = 1, col = color[2])
# 3rd plot
plot(1:length(log.err.slope[[3]]), log.err.slope[[3]], pch = 8, col = color[3], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.slope[[3]]))
# axis(side = 1, at = floor(seq(1, length(log.err.slope[[3]]), l = length.axis)))
lines(1:length(log.err.slope[[3]]), log.err.slope[[3]], lty = 1, col = color[3])
# Saving pdf (if fig.gen = TRUE)
if (fig.gen) dev.off()
#### Figure 6: Changing the volatility ####
# Reading data
load(file = "data/Volatility.RData")
# Plot settings
if (fig.gen) { # Setting for saving the PDF image
  pdf(paste0("img/volatility.pdf"), width = , height = 10)
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 3, 5, 
                  2, 4, 6,
                  7, 7, 7, 
                  8, 9, 10), nrow = 4, byrow = TRUE), heights = c(2, 2, 4, 2))
} else {
  par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
      mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
  layout(matrix(c(1, 3, 5, 
                  2, 4, 6,
                  7, 7, 7, 
                  8, 9, 10), nrow = 4, byrow = TRUE), heights = c(2, 2, 4, 2))
}
# Coefficients plot
for (i in 1:ncol(f1.t)) {
  # slope graph
  f1.t_ <- 1 / f1.t[, i] # Inverse f1.t
  f1.bridge.t_ <- 1 / f1.bridge.t[, i] # Inverse f1.bridge.t
  slopes <- cbind(f1.t_, f1.bridge.t_)
  matplot(t.line, slopes, type = "l", lty = c(1, 2), lwd = 2, 
          col = scales::alpha(color[i], c(0.5, 1)), xlab = "", ylab = "")
  lines(t.line, rep(0, length(t.line)), lty = 3, lwd = 2, col = "black")
  # drift graph
  f2.t[, i] <- f2.t[, i] / max(abs(f2.t[, i])) # Normalized f2.t
  f2.bridge.t[, i] <- f2.bridge.t[, i] / max(abs(f2.bridge.t[, i])) # Normalized f2.bridge.t
  level <- cbind(f2.t[, i], f2.bridge.t[, i])
  matplot(t.line, level, type = "l", lty = c(1, 2), lwd = 2, ylim = c(min(level), max(level)),
          col = scales::alpha(color[i], c(0.5, 1)), xlab = "", ylab = "")
  lines(t.line, rep(x.final, length(t.line)), lty = 3, lwd = 2, col = "black")
}
# Main plot
y.min.max <- range(bnd.vol)
matplot(t.line, bnd.vol, type = "l", lty = 1, ylim = c(y.min.max[1], y.min.max[2] + 2.3),
        xlab = "", ylab = "", col = color, lwd = 3)
# Main plot's legend
if (fig.gen) {
  legend(x = 0.43, y = 6.72, bty = "n", title = "",
         legend = c(TeX(paste0("$\\tilde{\\nu}(t) =\\, ", 3*c, " + ", 3*c , "\\Phi(100t -\\, 50)$")),
                    TeX(paste0("$\\tilde{\\nu}(t) =\\, ", 3*c, " + ", 2*c ,"\\cos(8\\pi t)$")),
                    TeX(paste0("$\\tilde{\\nu}(t) =\\, ", c, " + ", 5*c , "\\sqrt{2\\pi}\\phi(50t -\\, 25)$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2, y.intersp = 1.3)
} else {
  legend(x = 0.57, y = 4, bty = "n", title = "",
         legend = c(TeX(paste0("$\\tilde{\\nu}(t) = $", 3*c, "$ + $", 3*c , "$\\Phi(100t - 50)$")),
                    TeX(paste0("$\\tilde{\\nu}(t) = $", 3*c, "$ + $", 2*c ,"$\\cos(8\\pi t)$")),
                    TeX(paste0("$\\tilde{\\nu}(t) = $", c, "$ + $", 5*c , "$\\sqrt{2\\pi}\\phi(50t - 25)$"))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}
# Errors' plots
err.min.max <- range(log.err.vol)
# 1st error plot
plot(1:length(log.err.vol[[1]]), log.err.vol[[1]], pch = 8, col = color[1], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.vol[[1]]))
# axis(side = 1, at = floor(seq(1, length(log.err.vol[[1]]), l = length.axis)))
lines(1:length(log.err.vol[[1]]), log.err.vol[[1]], lty = 1, col = color[1])
# 2nd error plot
plot(1:length(log.err.vol[[2]]), log.err.vol[[2]], pch = 8, col = color[2], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.vol[[2]]))
# axis(side = 1, at = floor(seq(1, length(log.err.vol[[2]]), l = length.axis)))
lines(1:length(log.err.vol[[2]]), log.err.vol[[2]], lty = 1, col = color[2])
# 3rd plot
plot(1:length(log.err.vol[[3]]), log.err.vol[[3]], pch = 8, col = color[3], 
     ylim = c(err.min.max[1], err.min.max[2]), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = 1:length(log.err.vol[[3]]))
# axis(side = 1, at = floor(seq(1, length(log.err.vol[[3]]), l = length.axis)))
lines(1:length(log.err.vol[[3]]), log.err.vol[[3]], lty = 1, col = color[3])
# Saving pdf (if fig.gen = TRUE)
if (fig.gen) dev.off()


























