populate.bma = function(bma.dataset, f_synch_index, country_index, synch_index, date_index, 
                        d_2010_index, d_2011_index, d_2012_index, d_2013_index){ 
  
  
  for (dd in 1:dim(bma.dataset)[1]) {
    
    if (dd %in% seq(2,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.1, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2010_index, d_2011_index, d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(3,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.2, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2011_index, d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(4,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.3, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2011_index, d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(5,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.4, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2011_index, d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(6,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.5, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2011_index, d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(7,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.6, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(8,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.7, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(9,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.8, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(10,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.9, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                         country_index, synch_index, date_index,
                                                                                         d_2012_index, d_2013_index)])
      
    }
    
    if (dd %in% seq(11,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.10, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                          country_index, synch_index, date_index,
                                                                                          d_2013_index)])
      
    }
    
    if (dd %in% seq(12,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.11, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                          country_index, synch_index, date_index,
                                                                                          d_2013_index)])
      
    }
    
    if (dd %in% seq(13,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.12, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                          country_index, synch_index, date_index,
                                                                                          d_2013_index)])
      
    }
    
    if (dd %in% seq(14,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.13, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                          country_index, synch_index, date_index,
                                                                                          d_2013_index)])
      
    }
    
    if (dd %in% seq(15,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.14, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                          country_index, synch_index, date_index)])
      
    }
    
    if (dd %in% seq(16,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.15, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                          country_index, synch_index, date_index)])
      
    }
    
    if (dd %in% seq(17,final.seq,len.seq)) {
      
      bma.dataset[dd,f_synch_index] =
        predict(bma.model.synch.levels.16, newdata = bma.dataset[dd,-c(f_synch_index,
                                                                                          country_index, synch_index, date_index)])
      
    }
  }
  
  
  bma.dataset %>%
    select(date, country, synch, f_synch)
  
  
  
  }