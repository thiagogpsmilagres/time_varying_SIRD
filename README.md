## R codes for the time varying SIRD

- These codes are used to generate the effective reproduction number, Rt, shown in the [covid19analytics.com.br](https://covid19analytics.com.br/) website. 

- A technical note can be found [here](https://covid19analytics.com.br/publicacoes/nota-tecnica-atualizada-para-o-numero-de-reproducao-r/)

- Run setup.R to install the required packages and run the auxiliary functions.

- The data folder has a sample dataset.

- Given known problems such as the short sample, measurement errors, and structural breaks, we cannot guarantee that the model will perform well under all contingencies possible. Needless to say, we are not responsible for any misuse of these codes. 

- We aim to keep developing the method and updating these codes.

## SIRD example

- After loading df_BR from the data folder, you can run the following example:

```{r}
results <- run_SIRD(df = df_BR, size_population = unique(df_BR$pop))
```