Tstar %<>%
  mutate(Tstar0 = case_when(
    genotype == "nhr-52" ~ `T*` - 0.63,
    TRUE ~ `T*`),
    Tstar.1 = case_when(
      genotype == "nhr-52" ~ `T*` - 0.53,
      TRUE ~ `T*`),
    Tstar.2 = case_when(
      genotype == "nhr-52" ~ `T*` - 0.43,
      TRUE ~ `T*`),
    Tstar.3 = case_when(
      genotype == "nhr-52" ~ `T*` - 0.33,
      TRUE ~ `T*`),
    Tstar.4 = case_when(
      genotype == "nhr-52" ~ `T*` - 0.23,
      TRUE ~ `T*`),
    Tstar.5 = case_when(
      genotype == "nhr-52" ~ `T*` - 0.13,
      TRUE ~ `T*`))

lm.group.0 <- Tstar %>% lmerTest::lmer(data = ., Tstar.0 ~ genotype + (1|group)) 
lm.group.0 %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
sim_power.0 <- simr::powerSim(lm.group.0, nsim = 100)

lm.group.1 <- Tstar %>% lmerTest::lmer(data = ., Tstar.1 ~ genotype + (1|group)) 
lm.group.1 %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
sim_power.1 <- simr::powerSim(lm.group.1, nsim = 100)

lm.group.2 <- Tstar %>% lmerTest::lmer(data = ., Tstar.2 ~ genotype + (1|group)) 
lm.group.2 %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
sim_power.2 <- simr::powerSim(lm.group.2, nsim = 100)

lm.group.3 <- Tstar %>% lmerTest::lmer(data = ., Tstar.3 ~ genotype + (1|group)) 
lm.group.3 %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
sim_power.3 <- simr::powerSim(lm.group.3, nsim = 100)

lm.group.4 <- Tstar %>% lmerTest::lmer(data = ., Tstar.4 ~ genotype + (1|group)) 
lm.group.4 %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
sim_power.4 <- simr::powerSim(lm.group.4, nsim = 100)

lm.group.5 <- Tstar %>% lmerTest::lmer(data = ., Tstar.5 ~ genotype + (1|group)) 
lm.group.5 %>% emmeans::emmeans("genotype") %>% 
  emmeans::contrast(method = "pairwise")
sim_power.5 <- simr::powerSim(lm.group.5, nsim = 100)

sim_power

