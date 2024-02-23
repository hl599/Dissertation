* Clear the Stata environment and all Mata objects and matrices
clear
clear mata
clear matrix

* Start Mata environment for matrix programming
mata:

void GMM_DLW_CD(todo, betas, crit, g, H)
{
    PHI = st_data(., ("phi"))
    PHI_LAG = st_data(., ("phi_lag"))
    Z = st_data(., ("const", "m_lag", "l_lag", "k"))
    X = st_data(., ("const", "m", "l", "k"))
    X_lag = st_data(., ("const", "m_lag", "l_lag", "k_lag"))
    Y = st_data(., ("q"))
    C = st_data(., ("const"))

    OUTPUTTARIFF = st_data(., "tariff_output")
    INPUTTARIFF = st_data(., "tariff_input")
    EXPORT = st_data(., "expdummy")

    OMEGA = PHI - X * betas'
    OMEGA_lag = PHI_LAG - X_lag * betas'
    OMEGA_lag_pol = (C, EXPORT, OUTPUTTARIFF, INPUTTARIFF, OMEGA_lag)
    g_b = invsym(OMEGA_lag_pol'OMEGA_lag_pol) * OMEGA_lag_pol'OMEGA
    XI = OMEGA - OMEGA_lag_pol * g_b
    crit = (Z'XI)'(Z'XI)
}

void DLW_CD()
{
    initialvalue = st_data(1, ("initialConst", "initialm", "initiall", "initialk"))
    S = optimize_init()
    optimize_init_evaluator(S, &GMM_DLW_CD())
    optimize_init_evaluatortype(S, "d0")
    optimize_init_technique(S, "nm")
    optimize_init_nmsimplexdeltas(S, 0.1)
    optimize_init_which(S, "min")
    optimize_init_params(S, initialvalue)
    p = optimize(S)
    p
    st_matrix("beta_DLW_CD", p)
}

end

capture program drop DLW_CD
program DLW_CD, rclass
    preserve
    sort firm year
    mata DLW_CD()
end
