# CMU Fall 2020
# Professional Speaking
# Dear data presentation
# Alejandro Álvarez
# 09/27/2020

library(tidyverse)

##### Dataframe  and plot generator functions #####

f_coffee_stain <- function(n) {

    dat <- tibble(r = c(),theta = c(), d = c(), alpha = c(), color = c())

    for (i in 1:n) {

        dat <- dat %>%
            add_row(r = 1 + runif(1,0,0.17),
                    theta = runif(1,0.13, 2 * pi - 0.57),
                    d = runif(1,0.1,10),
                    alpha = runif(1,0,0.05),
                    color = sample(1:length(colors),1))
    }

    for (i in 1:n) if (dat$theta[i] < 0.17 * pi) dat$theta[i] <- dat$theta[i] - runif(1,0,0.13) * pi + runif(1,0,0.03) * pi
    for (i in 1:n) if (dat$theta[i] > 1.73 * pi) dat$theta[i] <- dat$theta[i] + runif(1,0,0.13) * pi - runif(1,0,0.03) * pi

    for (i in 1:n) dat$r[i] <- dat$r[i] + runif(1,0,0.011) - runif(1,0,0.07)

    dat <- dat %>% arrange(theta)

    return(dat)

}

f_coffee_plot <- function(file, n, width = 5,height = 5) {

    sysfonts::font_add('apple', 'HomemadeApple-Regular.ttf')
    showtext::showtext_auto()

    coffee_img <- png::readPNG('coffee_stain.png')
    coffee_rg <- grid::rasterGrob(coffee_img)

    colors <- c('#d8b07d','#f3d3aa','#be8b4e','#faf1e2','#f4dcc4',
                '#c5945c','#f3dcb8','#eccc96','#cc9c62','#c49c64')

    dat <- f_coffee_stain(n) %>%
        mutate(x = r * cos(theta), y = r * sin(theta)) %>%
        select(-r,-theta) %>%
        add_row(x = -0.27, y = -0.515, d = 7, alpha = 0.32, color = 3)

    test <- dat %>%
        ggplot(aes(x = x, y = y, size = d, alpha = alpha, fill = as.factor(color))) +
        annotation_custom(coffee_rg, xmin = 0.55, xmax = Inf, ymin = 0.35, ymax = Inf) +
        annotate('text', 0, 0, family = 'apple', size = 20, color = '#453710',
                 label = glue::glue('{floor(n/1000)},{n %% 1000} coffee beans')) +
        annotate('text', 0.17, -0.5, family = 'apple', size = 12, color = '#453710',
                 label = glue::glue(' = 1 coffee bean')) +
        annotate('text', 1.20, -0.15, size = 4.5, color = '#453710', angle = 90,
                 label = '\n\nVisualization: Alejandro Álvarez') +
        geom_point(shape = 16, color = '#be8a50') +
        scale_x_continuous(limits = c(-(max(abs(dat$x)) * 1.1),max(abs(dat$x)) * 1.1)) +
        scale_y_continuous(limits = c(-(max(abs(dat$y)) * 1.1),max(abs(dat$y)) * 1.1)) +
        scale_alpha_continuous(limits = c(0,1)) +
        scale_size_continuous(limits = c(0,15)) +
        scale_fill_manual(values = colors) +
        theme_minimal() +
        theme(legend.position ='none',
              panel.background = element_blank(),
              plot.background = element_rect(color = NA, fill = '#eae6df'),
              panel.grid = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              plot.caption = element_text(hjust = 0, size = 15, color = '#453710'),
              plot.title = element_text(vjust = -2,hjust = 0.5, family = 'apple', size = 40, color = '#453710'),
              plot.tag = element_text(hjust = 1, vjust = 1, color = '#453710')) +
        labs(title = 'I\'ll get you up and going out of bed',
             caption = 'Number of ground and brewed coffee beans between Wednesday, September 23, 2020, 19:11, and Sunday, September 27, 2020, 19:09.')

    ggsave(file, test,bg = 'transparent',width = width, height = height)

}

##### plot #####

n <- 216 + 214 + 222 + 211 + 233 + 235 + 250 + 244
f_coffee_plot(file = 'test.png',n, width = 4.6, height = 5)
