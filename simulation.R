library(tidyverse)
library(magrittr)
library(furrr)
library(tictoc)


set.seed(123)


# R_tib <- tibble( R = c(0.5, 0.69, 0.8, 0.10, 0.12) )
# 
# Tf_factor_tib <- tibble( Tf_factor = c(1.05, 1.1, 1.2, 1.3) )
# 
# Tk_factor_tib <- tibble( Tk_factor = c(0.6, 0.75, 0.8, 0.9) )
# 
# Rf_factor_tib <- tibble( Rf_factor = c(1.05, 1.1, 1.2, 1.3) )
# 
# Rk_factor_tib <- tibble( Rk_factor = c(0.6, 0.75, 0.8, 0.9) )
# 
# Qk_factor_tib <- tibble( Qk_factor = c(0.02, 0.035, 0.5, 0.75, 0.1) )

R_tib <- tibble( R = c(  0.069 ) )

Tf_factor_tib <- tibble( Tf_factor = c(1.05, 1.1,  1.3) )

Tk_factor_tib <- tibble( Tk_factor = c(0.6, 0.75, 0.9) )

Rf_factor_tib <- tibble( Rf_factor = c(1.05, 1.1,  1.3) )

Rk_factor_tib <- tibble( Rk_factor = c(0.6, 0.75,  0.9) )

Qk_factor_tib <- tibble( Qk_factor = c(  0.5, 0.6) )


parameters <- tibble(
    T = c(29.79), #resolution time
    N = c(360), #iteration duration
    D = c(2), # number of developers
    I = 1, #work item arrival probability
) %>% 
    crossing(R_tib) %>% 
    crossing(Tf_factor_tib) %>% 
    crossing(Tk_factor_tib) %>% 
    crossing(Rf_factor_tib) %>% 
    crossing(Rk_factor_tib) %>% 
    crossing(Qk_factor_tib)







strategies <- tibble(
    id = 0:1,
    strategy = c("always f", "always k"),
    pf = c(1,0),
    pk = c(0,1)
)

events <- tibble(
    game = integer()
)


parameters %<>%
    mutate(
        Tf = T * Tf_factor,
        Rf = R * Rf_factor,
        Tk = T * Tk_factor,
        Rk = R * Rk_factor
    )
    
#simulate one game with one strategy per dev
simulate_game <- function(...)
{
    this_game <- list(...)

    game <- this_game[[1]]$game_id
    
    
    simulation <- this_game[[1]]$simulation
    
    

    
    N <- this_game[[1]]$N 
    I <- this_game[[1]]$I 
    T <- this_game[[1]]$T 
    R <- this_game[[1]]$R 
    Tk <- this_game[[1]]$Tk 
    Tf <- this_game[[1]]$Tf 
    Rk <- this_game[[1]]$Rk 
    Rf <- this_game[[1]]$Rf 
    Qk_factor = this_game[[1]]$Qk_factor
    
    now <-  0
    n_to_do <- 0
    

    devs_work <- this_game[[1]] %>% 
        select_at(vars(num_range(prefix = "", range = 0:1000))) %>%  
        gather(dev, strategy) %>% 
        left_join(strategies, by = c("strategy"= "id") ) %>% 
        mutate(
            idle = TRUE,
            completed_items = 0,
            next_completion = 0.00001,
            reworking = FALSE,
            action="0",
            time_to_complete = NA
        )
    
    
    while(now <= N ){
        
        
        
        #Finishing (or setting rework on) current items
        devs_work %<>% 
            mutate(
                work_completed_now = next_completion <= now                    
                ,
                rework_current_item = 
                    if_else(
                        idle,
                        0L,
                        if_else(
                            action == "k",
                            rbinom(1, 1, Rk),
                            rbinom(1, 1, Rf)
                        )
                    )
                ,
                completed_items =
                    if_else(
                        work_completed_now & !rework_current_item & !idle,
                        completed_items + 1,
                        completed_items
                    )
                ,
                next_completion =
                    if_else(
                        as.logical(work_completed_now),
                        if_else(
                            as.logical(rework_current_item),
                            next_completion + Tf,
                            next_completion
                        ),
                        as.numeric(next_completion)
                    )
                ,
                idle =
                    if_else(
                        work_completed_now,
                        !rework_current_item,
                        idle
                    ),
                reworking =
                    if_else(
                        work_completed_now,
                        rework_current_item,
                        as.integer(reworking)
                    )
            ) %>% 
            mutate_at(
                vars(
                    one_of(
                        c(
                        "work_completed_now",
                        "rework_current_item",
                        "idle",
                        "reworking"
                        )
                    )
                ),
                as.logical
            )
        
        
        
        completed_k <- devs_work %>% 
            filter(work_completed_now & action == "k" & !reworking ) %>% 
            nrow()
        
        Tk <- Tk * (1 + Qk_factor)^completed_k
        Tf <- Tf * (1 + Qk_factor)^completed_k
        
        completed <- devs_work %>% 
            filter(work_completed_now & !rework_current_item) %>% 
            nrow()
        
        n_to_do <- n_to_do - completed
        
        
        #taking next items from to do list
        
        devs_idle <- devs_work %>% 
            filter(idle)
        
        n_to_do <- n_to_do + rbinom(1, 1, I)


        devs_to_do <- devs_idle %>% 
            sample_n( min(n_to_do,nrow(devs_idle)) ) %>% 
            mutate(
                action = if_else(rbinom(1, 1, pk) == 1, "k", "f")
            ) %>% 
            mutate(
                time_to_complete = if_else(action == "k", Tk, Tf )
            )
        
        
        devs_work %<>%
            left_join(devs_to_do, by = c("dev"="dev"), suffix = c("","_todo"),  ) %>%
            mutate(achou = !is.na(action_todo)) %>% 
            mutate(
                action = 
                    if_else(
                        achou,
                        action_todo,
                        action
                    ),
                idle =
                    if_else(
                        achou,
                        FALSE,
                        idle
                    ),
                reworking =
                    if_else(
                        achou,
                        FALSE,
                        reworking
                    ),
                next_completion =
                    if_else(
                        achou,
                        next_completion + time_to_complete_todo,
                        next_completion
                    ),
            ) %>% 
            select_at(vars(-contains("_todo")))
        

        
        log_devs_work <- devs_work %>% 
            mutate(
                now = now,
                Tf = Tf,
                Tk = Tk,
                game = game,
                simulation = simulation
            )
        
        events <<- events %>% 
            bind_rows(log_devs_work)
        
        next_completion <- min(devs_work$next_completion)
        
        add_to_do <- rbinom(1, floor(next_completion - now), I) %>% 
            sum()
        
        n_to_do <-  n_to_do + add_to_do
        
        
        
        
        now <-  max(now + 1, ceiling(next_completion))

    }
    
    # print(devs_work)
    
    devs_work
}


#simulate games for instance of parameters


#generate all games: all combinations of developers adopting strategies

simulate <- function(...)
{
   params <- list(...)

   
   # T = c(29.79), #resolution time
   # N = c(360), #iteration duration
   # D = c(2), # number of developers
   # I = 1, #work item arrival probability
   # R = 0.69, #rework probability
   # Tf_factor = 1.1, #factor do be multiplied by T to make T_f,
   # Rf_factor = 0.9,
   # Tk_factor = 0.75,
   # Rk_factor = 1.05,
   # Qk_factor = 0.05

   N <- params[[1]]$N
   D <- params[[1]]$D
   I <- params[[1]]$I
   Tf <- params[[1]]$Tf
   Tk <- params[[1]]$Tk
   Rf <- params[[1]]$Rf
   Rk <- params[[1]]$Rk
   Qk <- params[[1]]$Qk_factor
   T <- params[[1]]$T
   R <- params[[1]]$R
   simulation <- params[[1]]$simulation
   id_param <- params[[1]]$id_param
   
   print(paste0("param:", id_param, "simul", simulation))
   
   Now <- 0

   n_strategies <- nrow(strategies)
   
   
   params_tibble <- params[[1]]
   
   #Creating the set of possible strategies
   

   games <- 
       tibble(
           game = 0:(n_strategies^D-1)
       ) %>%  
       crossing(
          tibble(dev = 0:(D-1))
       ) %>% 
       mutate(
            strategy = game %/% n_strategies^dev %% n_strategies
       ) %>% 
       spread(dev, strategy) %>% 
       crossing(params_tibble) %>% 
       mutate(game_id = game) %>% 
       mutate(simulation = simulation) %>% 
       group_by(game) %>% 
       nest() %>% 
       mutate(data = map(data, simulate_game)) %>% 
       mutate(
           N = N,
           D = D,
           I = I,
           Tf = Tf,
           Tk = Tk,
           Rf = Rf,
           Rk = Rk,
           Qk = Qk,
           T = T,
           R = R,

           
       ) %>% 
       unnest()

   
   # print(simulation)
   
   games
   

   
}    
    
n_simulations <- 5
    

tic()

parameters %<>%
    mutate(id_param = row_number()) %>% 
    crossing(tibble(simulation = 1:n_simulations)) %>% 
    mutate(id_simul = row_number(), simulation_out = simulation, id_param_out = id_param) %>% 
    group_by(id_param_out, simulation_out) %>%  
    nest() %>% 
    mutate(data = future_map(data, simulate, .progress = TRUE)) %>% 
    unnest()

toc()

# 
# games_results <-  parameters %>% 
#     group_by(id_param, game, dev, strategy.y) %>% 
#     summarise(completed_items = mean(completed_items)) %>% 
#     ungroup() %>% 
#     mutate(dev = paste0("dev", as.character(dev))) %>% 
#     mutate(resultado = paste0(dev, " = ", as.character(completed_items))) %>% 
#     group_by(game) 
# 
# 
# saveRDS(parameters, "cache/parameters.rds")
# 
# saveRDS(games_results , "cache/games_results.rds")
# 




