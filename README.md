# Executable Environment for OSF Project [2phst](https://osf.io/2phst/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Social Space Diffusion: Applications of a Latent Space Model to Diffusion with Uncertain Ties

**Project Description:**
> Social networks represent two different facets of social life: (1) stable paths for diffusion, or the spread of something through a connected population, and (2) random draws from an underlying social space, which indicate the relative positions of the people in the network to one another.  The dual nature of networks creates a challenge – if the observed network ties are a single random draw, is it realistic to expect that diffusion only follows the observed network ties?  This study takes a first step towards integrating these two perspectives by introducing a social space diffusion model.  In the model, network ties indicate positions in social space, and diffusion occurs proportionally to distance in social space.  Practically, the simulation occurs in two parts.  First, positions are estimated using a statistical model (in this example, a latent space model).  Then, second, the predicted probabilities of a tie from that model – representing the distances in social space – or a series of networks drawn from those probabilities – representing routine churn in the network – are used as weights in a weighted averaging framework.  Using longitudinal data from high school friendship networks, I explore the properties of the model.  I show that the model produces smoothed diffusion results, which predict attitudes in future waves 10% better than a diffusion model using the observed network, and up to 5% better than diffusion models using alternative, non-model-based smoothing approaches.

**Original OSF Page:** [https://osf.io/2phst/](https://osf.io/2phst/)

---

**Important Note:** The contents of the `2phst_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_2phst-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_2phst-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `2phst_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-2phst-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-2phst-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_2phst](https://github.com/code-inspect-binder/osf_2phst)

