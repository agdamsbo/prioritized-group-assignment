group_assignment <-
  function(ds,
           cap_classes = NULL,
           excess_space = NULL,
           pre_assign = NULL) {
    require(ROI)
    require(ROI.plugin.symphony)
    
    if (!is.data.frame(ds)){
      stop("Supplied data has to be a data frame, with each row
           are subjects and columns are groups, with the first column being
           subject identifiers")}
    
    ## This program very much trust the user to supply correctly formatted data
    cost <- t(ds[,-1]) #Transpose converts to matrix
    colnames(cost) <- ds[,1]
    
    num_groups <- dim(cost)[1]
    num_sub <- dim(cost)[2]
    
    ## Adding the option to introduce a bit of head room to the classes by
    ## the groups to a little bigger than the smallest possible
    ## Default is to allow for an extra 20 % fill
    if (is.null(excess_space)) {
      excess <- 1.2
    } else {
      excess <- excess_space
    }
    
    # generous round up of capacities
    if (is.null(cap_classes)) {
      capacity <- rep(ceiling(excess*num_sub/num_groups), num_groups)
    # } else if (!is.numeric(cap_classes)) {
    #   stop("cap_classes has to be numeric")
    } else if (length(cap_classes)==1){
     capacity <- ceiling(rep(cap_classes,num_groups)*excess)
    } else if (length(cap_classes)==num_groups){
      capacity <- ceiling(cap_classes*excess)
    } else {
      stop("cap_classes has to be either length 1 or same as number of groups")
    }
    
    ## This test should be a little more elegant
    ## pre_assign should be a data.frame or matrix with an ID and assignment column
    with_pre_assign <- FALSE
    if (!is.null(pre_assign)){
      # Setting flag for later and export list
      with_pre_assign <- TRUE
      # Splitting to list for later merging
      pre <- split(pre_assign[,1],factor(pre_assign[,2],levels = seq_len(num_groups)))
      # Subtracting capacity numbers, to reflect already filled spots
      capacity <- capacity-lengths(pre)
      # Making sure pre_assigned are removed from main data set
      ds <- ds[!ds[[1]] %in% pre_assign[[1]],]
      
      cost <- t(ds[,-1])
      colnames(cost) <- ds[,1]
      
      num_groups <- dim(cost)[1]
      num_sub <- dim(cost)[2]
    }
    
    ## Simple NA handling. Better to handle NAs yourself!
    cost[is.na(cost)] <- num_groups
    
    i_m <- seq_len(num_groups)
    j_m <- seq_len(num_sub)
    
    m <- ompr::MIPModel() %>%
      ompr::add_variable(grp[i, j],
                   i = i_m,
                   j = j_m,
                   type = "binary") %>%
      ## The first constraint says that group size should not exceed capacity
      ompr::add_constraint(ompr::sum_expr(grp[i, j], j = j_m) <= capacity[i],
                     i = i_m) %>%
      ## The second constraint says each subject can only be in one group
      ompr::add_constraint(ompr::sum_expr(grp[i, j], i = i_m) == 1, j = j_m) %>%
      ## The objective is set to minimize the cost of the assignments
      ## Giving subjects the group with the highest possible ranking
      ompr::set_objective(ompr::sum_expr(
        cost[i, j] * grp[i, j],
        i = i_m,
        j = j_m
      ),
      "min") %>%
      ompr::solve_model(ompr.roi::with_ROI(solver = "symphony", verbosity = 1))
    
    ## Getting assignments
    solution <- ompr::get_solution(m, grp[i, j]) %>% filter(value > 0)
    
    assign <- solution |> select(i,j)
    
    if (!is.null(rownames(cost))){
      assign$i <- rownames(cost)[assign$i]
    }

    if (!is.null(colnames(cost))){
      assign$j <- colnames(cost)[assign$j]
    }
    
    ## Splitting into groups based on assignment
    assign_ls <- split(assign$j,assign$i)
    
    
    ## Extracting subject cost for the final assignment for evaluation
    if (is.null(rownames(cost))){
      rownames(cost) <- seq_len(nrow(cost))
    }
    
    if (is.null(colnames(cost))){
      colnames(cost) <- seq_len(ncol(cost))
    }
    
    eval <- lapply(seq_len(length(assign_ls)),function(i){
      ndx <- match(names(assign_ls)[i],rownames(cost))
      cost[ndx,assign_ls[[i]]]
    })
    names(eval) <- names(assign_ls)
    
    if (with_pre_assign){
      names(pre) <- names(assign_ls)
      assign_all <- mapply(c, assign_ls, pre, SIMPLIFY=FALSE)
      
      out <- list(all_assigned=assign_all)
    } else {
      out <- list(all_assigned=assign_ls)
    }
    
    export <- do.call(rbind,lapply(seq_along(out[[1]]),function(i){
      cbind("ID"=out[[1]][[i]],"Group"=names(out[[1]])[i])
    }))
    
    out <- append(out,
                  list(evaluation=eval,
                       assigned=assign_ls,
                       solution = solution,
                       capacity = capacity,
                       excess = excess,
                       pre_assign = with_pre_assign,
                       cost_scale = levels(factor(cost)),
                       input=ds,
                       export=export))
    # exists("excess")
    return(out)
  }


## Assessment performance overview
## The function plots costs of assignment for each subject in every group
assignment_plot <- function(lst){
  
  dl <- lst[[2]]
  cost_scale <- unique(lst[[8]])
  cap <- lst[[5]]
  cnts_ls <- lapply(dl,function(i){
    factor(i,levels=cost_scale)
  })
  require(ggplot2)
  require(patchwork)
  require(viridisLite)
  
  y_max <- max(lengths(dl))
  
  wrap_plots(lapply(seq_along(dl),function(i){
    ttl <- names(dl)[i]
    ns <- length(dl[[i]])
    cnts <- cnts_ls[[i]]
    ggplot2::ggplot() + ggplot2::geom_bar(ggplot2::aes(cnts,fill=cnts)) +
      ggplot2::scale_x_discrete(name = NULL, breaks=cost_scale, drop=FALSE) +
      ggplot2::scale_y_continuous(name = NULL, limits = c(0,y_max)) + 
      ggplot2::scale_fill_manual(values = viridisLite::viridis(length(cost_scale), direction = -1)) +
      ggplot2::guides(fill=FALSE) + 
      ggplot2::labs(title=paste0(ttl," (fill=",round(ns/cap[[i]],1),";m=",round(mean(dl[[i]]),1),";n=",ns ,")"))
  })) 
}


## Helper function for Shiny
file_extension <- function(filenames) {
  sub(pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "", filenames, perl = TRUE)
}



