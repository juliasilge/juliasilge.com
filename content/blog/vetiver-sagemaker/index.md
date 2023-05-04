---
title: "Deploy a model on AWS SageMaker with vetiver"
author: Julia Silge
date: '2023-05-04'
format: hugo
slug: vetiver-sagemaker
categories:
  - rstats
  - tidymodels
tags:
  - rstats
  - tidymodels
summary: "Learn how to train and deploy a model with R and vetiver on AWS SageMaker infrastructure."
---

This is the latest in my series of [screencasts](https://www.youtube.com/juliasilge)! This screencast walks through how to train and deploy a model using R and [vetiver](https://vetiver.rstudio.com/) on AWS SageMaker infrastructure. 🏺

{{% youtube "aeCcd-W1XXs" %}}

</br>

As of the time of publication, the [functions for interacting with SageMaker](https://rstudio.github.io/vetiver-r/dev/reference/index.html#sagemaker) from R are in the development version of vetiver; you will need to install from GitHub via `pak::pak("rstudio/vetiver-r")`. If you try out this new functionality, we would love [to get any feedback](https://github.com/rstudio/vetiver-r/issues) on how it went for you.

Here is the code I used in the video, for those who prefer reading instead of or in addition to video.

## Train a model

To get started quickly, let's use a dataset that many tidymodels users are very familiar with, the [Ames housing data](https://modeldata.tidymodels.org/reference/ames.html) that is used throughout [_Tidy Modeling with R_](https://www.tmwr.org/). If you have taken a look at our book, this model training will look very familiar:


```r
library(tidymodels)
data(ames)

set.seed(123)

ames_split <- 
  ames %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  mutate_if(is.integer, as.numeric) %>%
  initial_split(prop = 0.80, strata = Sale_Price)

ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

rf_spec <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude,
    rf_spec
  )

rf_fit <- rf_wflow %>% fit(data = ames_train)
```

## Create a deployable vetiver model

Our model is now trained, and it's time to create a deployable model object using vetiver. This example shows how to deploy a tidymodels object, but vetiver has support for [many types of models in R](https://rstudio.github.io/vetiver-r/dev/).


```r
library(vetiver)
v <- vetiver_model(rf_fit, "ames-pricing")
v
```

```
## 
## ── ames-pricing ─ <bundled_workflow> model for deployment 
## A ranger regression modeling workflow using 6 features
```

Notice that we have captured important characteristics of this model that we need for reliable deployment, like what the input predictor columns are like and the software dependencies of the model.

## Publish and version model on AWS S3

The next step is to store our model object as a [pin](https://pins.rstudio.com/dev/) in an S3 bucket. The functions from pins don't create a bucket, so we need to use an existing bucket here.


```r
library(pins)
## existing bucket:
identifier <- "sagemaker-vetiver-demo"

board <- board_s3(bucket = identifier)
vetiver_pin_write(board, v)
```

Using pins allows us [to version our model](https://vetiver.rstudio.com/get-started/version.html). If I trained this model again with new data on houses in Ames, I could store it as a new version of the same model.

## Build Docker container and deploy endpoint

There are three main tasks that need to be done to deploy this model as a SageMake endpoint:

- Generate and build a Docker image on SageMaker for a vetiver model 
- Create an Amazon SageMaker model 
- Deploy an Amazon SageMaker model endpoint 

There is a single function [`vetiver_deploy_sagemaker()`](https://rstudio.github.io/vetiver-r/dev/reference/vetiver_deploy_sagemaker.html) that takes care of all three of these for straightforward uses cases, but in this screencast, let's walk through how to use [more modular functions individually](https://rstudio.github.io/vetiver-r/dev/reference/vetiver_sm_build.html). Typically you would choose to do this instead of `vetiver_deploy_sagemaker()` if you needed more control of the deployment process.

The first function generates the files necessary to build a Docker container to deploy a vetiver model in SageMaker and then builds the image on [AWS CodeBuild](https://aws.amazon.com/codebuild/). The resulting image is stored in [AWS ECR](https://aws.amazon.com/ecr/). This step can take a while.


```r
new_image_uri <- vetiver_sm_build(board, "ames-pricing")
```

```
## * Lockfile written to 'vetiver_renv.lock'.
## [Container] 2023/05/03 22:02:50 Waiting for agent ping
## [Container] 2023/05/03 22:02:53 Waiting for DOWNLOAD_SOURCE
## [Container] 2023/05/03 22:02:55 Phase is DOWNLOAD_SOURCE
## [Container] 2023/05/03 22:02:55 CODEBUILD_SRC_DIR=/codebuild/output/src623956017/src
## [Container] 2023/05/03 22:02:55 YAML location is /codebuild/output/src623956017/src/buildspec.yml
## [Container] 2023/05/03 22:02:55 Setting HTTP client timeout to higher timeout for S3 source
## [Container] 2023/05/03 22:02:55 Processing environment variables
## [Container] 2023/05/03 22:02:55 No runtime version selected in buildspec.
## [Container] 2023/05/03 22:02:55 Moving to directory /codebuild/output/src623956017/src
## [Container] 2023/05/03 22:02:55 Configuring ssm agent with target id: codebuild:91967f79-f29e-4c47-8f58-26aef90251c2
## [Container] 2023/05/03 22:02:55 Successfully updated ssm agent configuration
## [Container] 2023/05/03 22:02:55 Registering with agent
## [Container] 2023/05/03 22:02:55 Phases found in YAML: 3
## [Container] 2023/05/03 22:02:55  PRE_BUILD: 9 commands
## [Container] 2023/05/03 22:02:55  BUILD: 4 commands
## [Container] 2023/05/03 22:02:55  POST_BUILD: 3 commands
## [Container] 2023/05/03 22:02:55 Phase complete: DOWNLOAD_SOURCE State: SUCCEEDED
## [Container] 2023/05/03 22:02:55 Phase context status code:  Message:
## [Container] 2023/05/03 22:02:55 Entering phase INSTALL
## [Container] 2023/05/03 22:02:55 Phase complete: INSTALL State: SUCCEEDED
## [Container] 2023/05/03 22:02:55 Phase context status code:  Message:
## [Container] 2023/05/03 22:02:55 Entering phase PRE_BUILD
## [Container] 2023/05/03 22:02:55 Running command echo Logging in to Amazon ECR...
## [Container] 2023/05/03 22:03:00 Phase complete: PRE_BUILD State: SUCCEEDED
## [Container] 2023/05/03 22:03:00 Phase context status code:  Message:
## [Container] 2023/05/03 22:03:00 Entering phase BUILD
## [Container] 2023/05/03 22:03:00 Running command echo Build started on `date`
## [Container] 2023/05/03 22:03:00 Running command echo Building the Docker image...
## [Container] 2023/05/03 22:03:00 Running command docker build -t $IMAGE_REPO_NAME:$IMAGE_TAG .
## [Container] 2023/05/03 22:05:04 Phase complete: POST_BUILD State: SUCCEEDED
## [Container] 2023/05/03 22:05:04 Phase context status code:  Message:
## julia-silge-rstudio-com-8ab: digest: sha256:ac67ab9aaf4681b3ac5bf6be7c36df2be2c4df32d608f87a7a1a440bec08d9f9 size: 2422
## 
## [Container] 2023/05/03 22:05:04 Phase complete: POST_BUILD State: SUCCEEDED
## [Container] 2023/05/03 22:05:04 Phase context status code:  Message:
## INFO [2023-05-03 22:05:22.243]: Image URI: 350573666743.dkr.ecr.us-east-2.amazonaws.com/sagemaker-studio-d-if5drhy5daez:julia-silge-rstudio-com-8ab
```

The second function creates a SageMaker model object from the Docker image. (This part is fast!)


```r
model_name <- vetiver_sm_model(new_image_uri)
```

The third function sets up a SageMaker endpoint from the newly created model. This step can also take a while, but you can set the argument `wait = FALSE` to free up your R session immediately (just be aware the endpoint won't be up for a while).


```r
new_endpoint <- vetiver_sm_endpoint(model_name, "ml.t2.medium")
new_endpoint
```

```
## 
## ── A SageMaker model endpoint for prediction: 
## Model endpoint: sagemaker-studio-d-if5drhy5daez-2023-05-03-22-05-22-711
## Region: us-east-2
```

Remember that you can use the single function `vetiver_deploy_sagemaker()` for straightforward use cases, but it can be helpful to understand what is going on under the hood like this. The `vetiver_deploy_sagemaker()` also has some nice quality-of-life features like setting some vetiver-specific tags and giving your ECR repo a vetiver-specific name, for discoverability.

## Make predictions with your deployed model

Once the endpoint is fully deployed, we can make predictions with it. You can see interactive documentation for the endpoint in the SageMaker UI under "Deployments" and then "Endpoints". You would input predictors as JSON, like this:

```json
[
  {
    "Neighborhood": "North_Ames",
    "Gr_Liv_Area": 1500,
    "Year_Built": 1960,
    "Bldg_Type": "OneFam",
    "Latitude": 42.0,
    "Longitude": -93.6
  },
  {
    "Neighborhood": "North_Ames",
    "Gr_Liv_Area": 2100,
    "Year_Built": 1995,
    "Bldg_Type": "OneFam",
    "Latitude": 42.0,
    "Longitude": -93.6
  } 
]
```

You can also get predictions with curl from the endpoint, or from R:


```r
new_homes <- ames_test %>% slice_sample(n = 50) 
predict(new_endpoint, new_homes)
```

```
## # A tibble: 50 × 1
##    .pred
##    <dbl>
##  1  5.01
##  2  4.92
##  3  5.12
##  4  5.03
##  5  5.14
##  6  5.08
##  7  5.25
##  8  4.92
##  9  5.31
## 10  5.60
## # ℹ 40 more rows
```

This new functionality for SageMaker is in the development version of vetiver as of publication, and we would love [to hear from you](https://github.com/rstudio/vetiver-r/issues) if you have feedback after trying it out!
