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

    reshape2

    fitdistrplus
    mixtools

    TSclust
    transport

    extRemes

    rstan

    corrr

    #ggplotr

    ggstats
    ggridges
    quantreg # for ggplot::stat_quantile()
    igraph # graph / state machines
    ggh4x

    furrr
    progressr

    tikzDevice
    svglite
    miniUI
    styler
  ];
  RStudio-with-packages = pkgs.rstudioWrapper.override { inherit packages; };
  R-with-packages = pkgs.rWrapper.override { inherit packages; };
in
{
  # https://devenv.sh/packages/
  packages = with pkgs; [
    #RStudio-with-packages
    R-with-packages
    curl.dev
    openssl
    libpng.dev # for tikzDevice
    fftw.dev

    yq
    nlohmann_json_schema_validator
  ];

  # https://devenv.sh/languages/
  languages.r = {
    enable = true;
    package = R-with-packages;
  };
}
