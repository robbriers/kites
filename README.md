# kites

R package for plotting kite diagrams from abundance data along a transect etc. Adapted from code here https://rpubs.com/thoughtfulbloke/kitegraph and packaged up with example data from Kames Bay, Millport.

Install via:

    # install 'remotes' package first if needs be using install.packages("remotes")
    library(remotes)
    install_github("robbriers/kites")

Example usage
    # load package
    library(kites)

    # load built in kames2016 data
    data(kames2016)

    # have a look at it
    str(kames2016)

    # plot the kite diagram, with taxa >20 total abundance
    plot_kite(kames2016, 20)
    
