fun <- function(egg) {
p <-  ggplot() + geom_point()
p <- p + switch(egg,
                aes(x = c(1,2,3), y=c(2,4,6)),
                aes(x = c(1,2,3), y=c(2,4,60)))

p
}