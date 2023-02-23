library(data.table)
library(lfe)
# library(fixest)


collect_results <- function(model, specification, spl, updn, package, outdtm, outdtt){
  
  contvec <- c('-',
               'country-birth-decade FE + child age FEs',
               '+ hh sz + I(mg hh) + #hh mbrs same gen, prev. gen, prev. gen w/o mom/pop + fam struct dummies + rel head dummies + pg aab dummies',
               '+ urban + prev. gen. industry + prev. gen. occupation',
               '+ district-urban FEs',
               'in above median closest parental education distribution districts: C v M',
               'in above median closest parental education distribution districts: C v T')
  
  
  
  if (specification==1) {
    startbetaserr <- 2
  } else {
    startbetaserr <- 1
  }
  
  if (package=='fixest') {
    a <- 1
    # b_m <- model$coefficients[startbetaserr]
    # b_t <- model$coefficients[startbetaserr+1]
    # se_m <- model$se[startbetaserr]
    # se_t <- model$se[startbetaserr+1]
    # nobs <- model$nobs
  } else if (package=='felm') {
    b_m <- model$coefficients[startbetaserr]
    b_t <- model$coefficients[startbetaserr+1]
    se_m <- sqrt(model$clustervcv[startbetaserr,startbetaserr])
    se_t <- sqrt(model$clustervcv[startbetaserr+1,startbetaserr+1])
    nobs <- model$N
  }
  
  outdtm[controls==contvec[specification] & sample==spl & direction==updn,`:=`(b=b_m, se=se_m, N=nobs)]
  outdtt[controls==contvec[specification] & sample==spl & direction==updn,`:=`(b=b_t, se=se_t, N=nobs)]
  
  # return(list('outm' = outdtm, 'outt' = outdtt))
  
}    


run_estimation <- function(formula, spl, updn, close, package) {
  if (package=='fixest') {
    a <- 1
    # if (spl=='all') {
    #   if (updn=='up') {
    #     if (close=='all') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up),wt], data=dt[!is.na(up)]), cluster= ~cbd)
    #     } else if (close=='mc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up) & similar_m==1,wt], data=dt[!is.na(up) & similar_m==1]), cluster= ~cbd)
    #     } else if (close=='tc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up) & similar_t==1,wt], data=dt[!is.na(up) & similar_t==1]), cluster= ~cbd)
    #     }
    #   } else if (updn=='down') {
    #     if (close=='all') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down),wt], data=dt[!is.na(down)]), cluster= ~cbd)
    #     } else if (close=='mc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down) & similar_m==1,wt], data=dt[!is.na(down) & similar_m==1]), cluster= ~cbd)
    #     } else if (close=='tc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down) & similar_t==1,wt], data=dt[!is.na(down) & similar_t==1]), cluster= ~cbd)
    #     }
    #   }
    # } else if (spl=='boys') {
    #   if (updn=='up') {
    #     if (close=='all') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up) & male==1,wt], data=dt[!is.na(up) & male==1]), cluster= ~cbd)
    #     } else if (close=='mc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up) & male==1 & similar_m==1,wt], data=dt[!is.na(up) & male==1 & similar_m==1]), cluster= ~cbd)
    #     } else if (close=='tc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up) & male==1 & similar_t==1,wt], data=dt[!is.na(up) & male==1 & similar_t==1]), cluster= ~cbd)
    #     }
    #   } else if (updn=='down') {
    #     if (close=='all') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down) & male==1,wt], data=dt[!is.na(down) & male==1]), cluster= ~cbd)
    #     } else if (close=='mc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down) & male==1 & similar_m==1,wt], data=dt[!is.na(down) & male==1 & similar_m==1]), cluster= ~cbd)
    #     } else if (close=='tc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down) & male==1 & similar_t==1,wt], data=dt[!is.na(down) & male==1 & similar_t==1]), cluster= ~cbd)
    #     }
    #   }
    # } else if (spl=='girls') {
    #   if (updn=='up') {
    #     if (close=='all') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up) & male==0,wt], data=dt[!is.na(up) & male==0]), cluster= ~cbd)
    #     } else if (close=='mc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up) & male==0 & similar_m==1,wt], data=dt[!is.na(up) & male==0 & similar_m==1]), cluster= ~cbd)
    #     } else if (close=='tc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(up) & male==0 & similar_t==1,wt], data=dt[!is.na(up) & male==0 & similar_t==1]), cluster= ~cbd)
    #     }
    #   } else if (updn=='down') {
    #     if (close=='all') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down) & male==0,wt], data=dt[!is.na(down) & male==0]), cluster= ~cbd)
    #     } else if (close=='mc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down) & male==0 & similar_m==1,wt], data=dt[!is.na(down) & male==0 & similar_m==1]), cluster= ~cbd)
    #     } else if (close=='tc') {
    #       m <- summary(feols(formula, weights=dt[!is.na(down) & male==0 & similar_t==1,wt], data=dt[!is.na(down) & male==0 & similar_t==1]), cluster= ~cbd)
    #     }
    #   }
    # }
  } else if (package=='felm') {
    if (spl=='all') {
      if (updn=='up') {
        if (close=='all') {
          m <- felm(formula, data=dt[!is.na(up)], weights=dt[!is.na(up),wt])
        } else if (close=='mc') {
          m <- felm(formula, data=dt[!is.na(up) & similar_m==1], weights=dt[!is.na(up) & similar_m==1,wt])
        } else if (close=='tc') {
          m <- felm(formula, data=dt[!is.na(up) & similar_t==1], weights=dt[!is.na(up) & similar_t==1,wt])
        }
      } else if (updn=='down') {
        if (close=='all') {
          m <- felm(formula, data=dt[!is.na(down)], weights=dt[!is.na(down),wt])
        } else if (close=='mc') {
          m <- felm(formula, data=dt[!is.na(down) & similar_m==1], weights=dt[!is.na(down) & similar_m==1,wt])
        } else if (close=='tc') {
          m <- felm(formula, data=dt[!is.na(down) & similar_t==1], weights=dt[!is.na(down) & similar_t==1,wt])
        }
      }
    } else if (spl=='boys') {
      if (updn=='up') {
        if (close=='all') {
          m <- felm(formula, data=dt[!is.na(up) & male==1], weights=dt[!is.na(up) & male==1,wt])
        } else if (close=='mc') {
          m <- felm(formula, data=dt[!is.na(up) & male==1 & similar_m==1], weights=dt[!is.na(up) & male==1 & similar_m==1,wt])
        } else if (close=='tc') {
          m <- felm(formula, data=dt[!is.na(up) & male==1 & similar_t==1], weights=dt[!is.na(up) & male==1 & similar_t==1,wt])
        }
      } else if (updn=='down') {
        if (close=='all') {
          m <- felm(formula, data=dt[!is.na(down) & male==1], weights=dt[!is.na(down) & male==1,wt])
        } else if (close=='mc') {
          m <- felm(formula, data=dt[!is.na(down) & male==1 & similar_m==1], weights=dt[!is.na(down) & male==1 & similar_m==1,wt])
        } else if (close=='tc') {
          m <- felm(formula, data=dt[!is.na(down) & male==1 & similar_t==1], weights=dt[!is.na(down) & male==1 & similar_t==1,wt])
        }
      }
    } else if (spl=='girls') {
      if (updn=='up') {
        if (close=='all') {
          m <- felm(formula, data=dt[!is.na(up) & male==0], weights=dt[!is.na(up) & male==0,wt])
        } else if (close=='mc') {
          m <- felm(formula, data=dt[!is.na(up) & male==0 & similar_m==1], weights=dt[!is.na(up) & male==0 & similar_m==1,wt])
        } else if (close=='tc') {
          m <- felm(formula, data=dt[!is.na(up) & male==0 & similar_t==1], weights=dt[!is.na(up) & male==0 & similar_t==1,wt])
        }
      } else if (updn=='down') {
        if (close=='all') {
          m <- felm(formula, data=dt[!is.na(down) & male==0], weights=dt[!is.na(down) & male==0,wt])
        } else if (close=='mc') {
          m <- felm(formula, data=dt[!is.na(down) & male==0 & similar_m==1], weights=dt[!is.na(down) & male==0 & similar_m==1,wt])
        } else if (close=='tc') {
          m <- felm(formula, data=dt[!is.na(down) & male==0 & similar_t==1], weights=dt[!is.na(down) & male==0 & similar_t==1,wt])
        }
      }
    }
  }    
  
  return(m)
  
}




prepare_formula <- function(specification, updn, package) {
  
  block1 <- ' + shrlit0rel1418'
  block2a <- ' + nhhmemb + mg +  n_sameg_r + n_sameg_n + n_prevg_r + n_prevg_1540 + n_prevg_r_nomompop + n_prevg_1540_nomompop'
  block2b <- ' + momonly + poponly + mompoponly + othersonly + rh_child + rh_biochild + rh_othchild + rh_grndchild + rh_head + rh_spouse + rh_sibl + rh_nonrel' 
  # note 'extended' is o.c. in momonly, poponly
  # 'rh_othrel' is o.c. in relationship to head
  block2c <- ' + amom_birth + apop_birth'
  block2 <- paste0(block2a, block2b, block2c)
  block3 <- ' + urban + ind0 + occ0'
  
  if (specification==1) {
    string_formula <- sprintf('%s ~ muslim + traditional', updn)
  } else if (specification==2) {
    string_formula <- sprintf('%s ~ muslim + traditional | cbd + age', updn)
  } else if (specification==3) {
    string_formula <- sprintf('%s ~ muslim + traditional%s | cbd + age', updn, block2)
  } else if (specification==4) {
    string_formula <- sprintf('%s ~ muslim + traditional%s%s | cbd + age', updn, block2, block3)
  } else if (specification %in% c(5, 6, 7)) {
    string_formula <- sprintf('%s ~ muslim + traditional%s%s | cbd + age + dur', updn, block2, block3)
  }
  
  if (package=='fixest') {
    a <- 1
    # formula <- as.formula(string_formula)
  } else if (package=='felm') {
    
    if (specification==1) {
      formula <- as.formula(paste0(string_formula, '| 0 | 0 | cbd'))
    } else {
      formula <- as.formula(paste0(string_formula, ' | 0 | cbd'))
    }
  }
  
  return(formula)
  
}
