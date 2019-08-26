library(tidyverse)
library(magrittr)



parameters <- tibble(
    T = c(29.79), #resolution time
    N = c(360), #iteration duration
    D = c(2), # number of developers
    I = 1, #work item arrival probability
    R = 0.69, #rework probability
    Tf_factor = 1.1, #factor do be multiplied by T to make T_f,
    Rf_factor = 0.9,
    Tk_factor = 0.75,
    Rk_factor = 1.05,
    Qk_factor = 0.05
)


strategies <- tibble(
    id = 0:1,
    strategy = c("always f", "always k"),
    pf = c(1,0),
    pk = c(0,1)
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

    N <- this_game[[1]]$N 
    I <- this_game[[1]]$I 
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
            next_completion = 0,
            reworking = FALSE,
            action="0",
            time_to_complete = NA
        )
    
    
    while(now <= N ){
        
        
        
        #Finishing (or setting rework on) current items
        devs_work %<>% 
            mutate(
                work_completed_now = next_completion < now                    
                ,
                rework_current_item = 
                    if_else(
                        reworking,
                        as.integer(0),
                        if_else(
                            action == "k",
                            rbinom(1, 1, Rk),
                            rbinom(1, 1, Rf)
                        )
                    )
                ,
                completed_items =
                    if_else(
                        work_completed_now & !rework_current_item,
                        completed_items + 1,
                        completed_items
                    )
                ,
                next_completion =
                    if_else(
                        as.logical(work_completed_now),
                        if_else(
                            as.logical(rework_current_item),
                            now + Tf,
                            now
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
            filter(work_completed_now & action == "k" ) %>% 
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
                        now + time_to_complete_todo,
                        next_completion
                    ),
            ) %>% 
            select_at(vars(-contains("_todo")))
        
        
        next_completion <- min(devs_work$next_completion)
        
        add_to_do <- rbinom(1, floor(next_completion - now), I) %>% 
            sum()
        
        n_to_do <-  n_to_do + add_to_do
        
        now <-  max(now + 1, ceiling(next_completion))

    }
    
    print(devs_work)
    
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
       group_by(game) %>% 
       nest() %>% 
       mutate(data = map(data, simulate_game)) %>% 
       unnest()

   
   games
   
}    
    

    
parameters %<>% 
    mutate(id_row = row_number()) %>% 
    group_by(id_row) %>%  
    nest() %>% 
    mutate(data = map(data, simulate)) %>% 
    unnest()









