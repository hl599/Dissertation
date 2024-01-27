* Clear the Stata environment and all Mata objects and matrices
clear
clear mata
clear matrix

* Start Mata environment for matrix programming
mata:

* Define a Mata function for GMM estimation in the context of DLW method
void GMM_DLW_CD(todo, betas, crit, g, H)
{
    * Retrieve data from Stata into Mata for analysis
    PHI = st_data(., ("phi"))
    PHI_LAG = st_data(., ("phi_lag"))
    Z = st_data(., ("const", "m_lag", "l_lag", "k"))
    X = st_data(., ("const", "m", "l", "k"))
    X_lag = st_data(., ("const", "m_lag", "l_lag", "k_lag"))
    Y = st_data(., ("q"))
    C = st_data(., ("const"))

    * Additional control variables
    OUTPUTTARIFF = st_data(., "tariff_output_l1")
    INPUTTARIFF = st_data(., "tariff_input_l1")
    EXPORT = st_data(., "expdummy")

    * Main GMM calculation steps
    OMEGA = PHI - X * betas'
    OMEGA_lag = PHI_LAG - X_lag * betas'
    OMEGA_lag_pol = (C, EXPORT, OUTPUTTARIFF, INPUTTARIFF, OMEGA_lag)
    g_b = invsym(OMEGA_lag_pol'OMEGA_lag_pol) * OMEGA_lag_pol'OMEGA
    XI = OMEGA - OMEGA_lag_pol * g_b
    crit = (Z'XI)'(Z'XI)
}

* Define a Mata function to initialize and run the optimization process
void DLW_CD()
{
    * Retrieve initial values for the optimization
    initialvalue = st_data(1, ("initialConst", "initialm", "initiall", "initialk"))
    * Initialize optimization settings
    S = optimize_init()
    optimize_init_evaluator(S, &GMM_DLW_CD())
    optimize_init_evaluatortype(S, "d0")
    optimize_init_technique(S, "nm")
    optimize_init_nmsimplexdeltas(S, 0.1)
    optimize_init_which(S, "min")
    optimize_init_params(S, initialvalue)
    * Run optimization and store results
    p = optimize(S)
    p
    * Return the optimized parameters to Stata
    st_matrix("beta_DLW_CD", p)
}

* End Mata environment
end

* Define a Stata program to call the Mata functions and perform the estimation
capture program drop DLW_CD
program DLW_CD, rclass
    preserve
    sort firm year
    * Execute the Mata function for estimation
    mata DLW_CD()
end
