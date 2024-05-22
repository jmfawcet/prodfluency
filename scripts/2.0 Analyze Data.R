
# Basic ANOVA of Experiment 1 ---------------------------------------------

#
# Including all participants
# 
e1_test_freq = ezANOVA(e1_sum %>% filter(aloud_silent_new != 'New'), .(acc), .(id), .(font_type, aloud_silent_new))
e1_test_freq$Descriptives = ezStats(e1_sum %>% filter(aloud_silent_new != 'New'), .(acc), .(id), .(font_type, aloud_silent_new))

e1_test_bf = anovaBF(acc ~ font_type*aloud_silent_new + id, data = e1_sum %>% filter(aloud_silent_new != 'New'),
                    whichRandom = "id", iterations=100000)
bayesfactor_inclusion(e1_test_bf, match_models=TRUE)

# Comparing False Alarms
t.test(acc~font_type, data=e1_sum %>% filter(aloud_silent_new == 'New'), paired=TRUE)
ttestBF(x=e1_sum %>% filter(aloud_silent_new == 'New', font_type=='alternating') %>% pull(acc), 
        y=e1_sum %>% filter(aloud_silent_new == 'New', font_type!='alternating') %>% pull(acc),
        paired=TRUE)

# SDT
e1_test_sdt_freq = ezANOVA(e1_sum_sdt, .(d), .(id), .(font, pe), detailed=TRUE)
e1_test_sdt_freq$Descriptives = ezStats(e1_sum_sdt, .(d), .(id), .(font, pe))

e1_test_sdt_bf = anovaBF(d ~ font*pe + id, data = e1_sum_sdt %>% mutate(id=factor(id), font=factor(font), pe=factor(pe)),
                     whichRandom = "id", iterations=100000)
bayesfactor_inclusion(e1_test_sdt_bf, match_models=TRUE)

#
# Excluding those not following instructions
#
e1_test_freq_nofol = ezANOVA(e1_sum %>% filter(aloud_silent_new != 'New', followed_instructions==1), .(acc), .(id), .(font_type, aloud_silent_new), detailed=TRUE)
e1_test_freq_nofol$Descriptives = ezStats(e1_sum %>% filter(aloud_silent_new != 'New', followed_instructions==1), .(acc), .(id), .(font_type, aloud_silent_new))

e1_test_bf_nofol = anovaBF(acc ~ font_type*aloud_silent_new + id, data = e1_sum %>% filter(aloud_silent_new != 'New', followed_instructions==1),
                     whichRandom = "id", iterations=100000)
bayesfactor_inclusion(e1_test_bf_nofol, match_models=TRUE)

# Comparing False Alarms
t.test(acc~font_type, data=e1_sum %>% filter(aloud_silent_new == 'New', followed_instructions == 1), paired=TRUE)
ttestBF(x=e1_sum %>% filter(aloud_silent_new == 'New', font_type=='alternating', followed_instructions == 1) %>% pull(acc), 
        y=e1_sum %>% filter(aloud_silent_new == 'New', font_type!='alternating', followed_instructions == 1) %>% pull(acc),
        paired=TRUE)

# Analysis of Experiment 2 ------------------------------------------------

#
# Study Phase
#

# Participant 36 has effectively no usable study phase trials, and is excluded
# from study phase analyses; however, these issues were technical and
# limited to the study phase, so with the exception of the footnote model
# excluding bad study phase trials, they remain included.

# Study Phase Accuracy
e2_studydat_raw %>% 
  filter(subject != 36) %>% # Had very few valid trials
  mutate(font = factor(font), production=factor(production), subject=factor(subject)) %>%
  group_by(subject, font, production) %>%
  summarize(acc = 1-mean(sacc)) -> e2_spk_sum

e2_result_spk_freq = ezANOVA(e2_spk_sum, .(acc), .(subject), .(font, production), detailed=TRUE)
e2_result_spk_freq$Descriptives = ezStats(e2_spk_sum, .(acc), .(subject), .(font, production))
e2_result_spk_bf = anovaBF(acc ~ font*production + subject, data = e2_spk_sum,
                           whichRandom = "subject")
bayesfactor_inclusion(e2_result_spk_bf, match_models=TRUE)

# Study Phase Reaction Times
e2_studydat_raw %>% 
  filter(subject != 36, !sinv) %>% # 36 had very few valid trials
  mutate(font = factor(font), production=factor(production), subject=factor(subject)) %>%
  group_by(subject, font, production) %>%
  summarize(log_srt = mean(log(srt)), srt = mean(srt)) -> e2_srt_sum
  
e2_result_rt_freq = ezANOVA(e2_srt_sum, .(srt), .(subject), .(font, production), detailed=TRUE)
e2_result_rt_freq$Descriptives = ezStats(e2_srt_sum, .(srt), .(subject), .(font, production))
e2_result_rt_bf = anovaBF(srt ~ font*production + subject, data = e2_srt_sum,
                          whichRandom = "subject", iterations=100000)
bayesfactor_inclusion(e2_result_rt_bf, match_models=TRUE)


#
# Old/New
#

e2_result_freq = ezANOVA(e2_sum %>% filter(production != 'New'), .(my), .(subject), .(font, production), detailed=TRUE)
e2_result_freq$Descriptives = ezStats(e2_sum %>% filter(production != 'New'), .(my), .(subject), .(font, production))

e2_result_bf = anovaBF(my ~ font*production + subject, data = e2_sum %>% filter(production != 'New'),
                       whichRandom = "subject", iterations=100000)
bayesfactor_inclusion(e2_result_bf, match_models=TRUE)

#
# Excluding Bad Study Phase Trials
# 

e2_sum_excs = e2_dat_raw %>%
  dplyr::select(subject, font, production, sresp, yes:f, sinv) %>%
  filter(subject != 36, !is.na(sinv), !sinv) %>%
  group_by(subject, font, production) %>%
  summarize(my = mean(yes), mr = mean(r), mf = mean(f), mfam = mean(f[r!=1])) %>%
  mutate(subject=factor(subject), font=factor(font), 
         production=factor(production))

e2_result_freq_excs = ezANOVA(e2_sum_excs, .(my), .(subject), .(font, production), detailed=TRUE)
e2_result_freq_excs$Descriptives = ezStats(e2_sum_excs, .(my), .(subject), .(font, production))

e2_result_bf_excs = anovaBF(my ~ font*production + subject, data = e2_sum_excs,
                       whichRandom = "subject", iterations=100000)
bayesfactor_inclusion(e2_result_bf_excs, match_models=TRUE)

#
# Analysis of Recollection
# 

e2_result_r_freq = ezANOVA(e2_sum %>% filter(production != 'New'), .(mr), .(subject), .(font, production))
e2_result_r_freq$Descriptives = ezStats(e2_sum %>% filter(production != 'New'), .(mr), .(subject), .(font, production))
e2_result_r_bf = anovaBF(mr ~ font*production + subject, data = e2_sum %>% filter(production != 'New'),
                         whichRandom = "subject", iterations=100000)
bayesfactor_inclusion(e2_result_r_bf, match_models=TRUE)

#
# Analysis of Familiarity
# 

e2_result_f_freq = ezANOVA(e2_sum %>% filter(production != 'New'), .(mfam), .(subject), .(font, production))
e2_result_f_freq$Descriptives = ezStats(e2_sum %>% filter(production != 'New'), .(mfam), .(subject), .(font, production))
e2_result_f_bf = anovaBF(mfam ~ font*production + subject, data = e2_sum %>% filter(production != 'New'),
                         whichRandom = "subject", iterations=100000)
bayesfactor_inclusion(e2_result_f_bf, match_models=TRUE)

# Combined Analysis of Experiments 1 + 2 ----------------------------------

e1_sum %>% 
  ungroup() %>%
  dplyr::select(id, font=font_type, pe=aloud_silent_new, acc=acc) %>%
  mutate(pe=str_trim(pe), exp = '1', id = paste(id, exp)) %>%
  bind_rows(e2_sum %>% 
              mutate(font = as.character(font)) %>%
              dplyr::select(id=subject, font, pe=production, acc=my) %>%
              mutate(pe=str_trim(pe), exp = '2', id = paste(id, exp), 
                     font = ifelse(font=='lower', 'lowercase', font))) %>%
  filter(pe != 'New') -> ecomb_sum

ecomb_test_freq = ezANOVA(ecomb_sum, .(acc), .(id), .(font, pe), between=.(exp), detailed=TRUE)
ecomb_test_freq$Descriptives = ezStats(ecomb_sum, .(acc), .(id), .(font, pe), between=.(exp))

ecomb_result_bf = anovaBF(acc ~ font*pe*exp + id, data = ecomb_sum %>% mutate(id=factor(id), font=factor(font), pe=factor(pe), exp=factor(exp)),
                         whichRandom = "id", iterations=100000)
bayesfactor_inclusion(ecomb_result_bf, match_models=TRUE)


