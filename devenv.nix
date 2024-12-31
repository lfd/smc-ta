{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

let
  packages = with pkgs.rPackages; [
    here
    tidyverse
    multidplyr
    tidymodels
    broom_mixed # converting bayesian models to tidy tibbles
    dotwhisker # visualizing regression results

    reshape2

    fitdistrplus
    mixtools

    TSclust
    transport

    extRemes

    rstan

    corrr

    qqconf
    #ggplotr

    ggstats
    ggridges
    quantreg # for ggplot::stat_quantile()
    igraph # graph / state machines
    ggh4x
    treemapify

    furrr
    progressr

    tikzDevice
    svglite
    miniUI
    styler
  ];
  RStudio-with-packages = pkgs.rstudioWrapper.override { inherit packages; };
  R-with-packages = pkgs.rstudioWrapper.override { inherit packages; };
in
{
  # https://devenv.sh/packages/
  packages = with pkgs; [
    RStudio-with-packages
    curl.dev
    openssl
    libpng.dev # for tikzDevice
    fftw.dev
  ];

  # https://devenv.sh/languages/
  languages.r = {
    enable = true;
    package = R-with-packages;
  };
}
