boa.plot                 package:boa                 R Documentation

_P_l_o_t _P_a_r_a_m_e_t_e_r_s _i_n _M_C_M_C _S_e_q_u_e_n_c_e_s

_D_e_s_c_r_i_p_t_i_o_n:

     Automatically generates the specified plot type for all parameters
     in the  working session list of MCMC sequences. This function
     takes care of all the  plotting tasks - opening windows, setting
     the number of plots per page, and  adding titles.

_U_s_a_g_e:

     boa.plot(type, dev = boa.par("dev"), mfdim = boa.par("plot.mfdim"),
     newplot = boa.par("plot.new"), onelink = boa.par("plot.onelink"),
     title = boa.par("title"))

_A_r_g_u_m_e_n_t_s:

    type: The type of plots to generate. The supported types are:

             *  "acf" = autocorrelation functions

             *  "bandg" = Brooks and Gelman multivariate shrink factors

             *  "density" = density functions

             *  "gandr" = Gelman and Rubin shrink factors

             *  "geweke" = Geweke convergence diagnostics

             *  "history" = running means

             *  "trace" = trace histories

     dev: Character string giving the name of the function that creates
           graphics windows on the current platform. For Unix systems
          this is either  "motif", "openlook", or "X11". For windows,
          this should be set to "win.graph"

   mfdim: Numeric vector giving the maximum number of rows and columns,
           respectively, of plots to include in a single graphics
          window. If only one  graphics window is opened, 'mfdim' is
          proportionately scaled down so as to  minimize the number of
          empty frames within that window.

 newplot: Logical value indicating that a new graphics window be 
          automatically opened. Otherwise, previous graphics windows
          will be closed.

 onelink: Logical value indicating that each plot should include only 
          one MCMC sequence. Otherwise, all sequences are displayed on
          the same plot.

   title: Logical value indicating that a title be added to the plot.

_V_a_l_u_e:

     A logical value indicating that the plots were successfully
     created.

_A_u_t_h_o_r(_s):

     Brian J. Smith

_S_e_e _A_l_s_o:

     'boa.plot.acf', 'boa.plot.bandg', 'boa.plot.density',
     'boa.plot.gandr', 'boa.plot.geweke', 'boa.plot.history',
     'boa.plot.trace'

