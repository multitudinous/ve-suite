#include "Prekin_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Prekin_UI_Dialog, UIDialog);

BEGIN_EVENT_TABLE(PrekinTabs, wxNotebook)
  EVT_RADIOBUTTON(PORE_MODEL1, PrekinTabs::OnPoreModel)
  EVT_RADIOBUTTON(PORE_MODEL2, PrekinTabs::OnPoreModel)
  EVT_RADIOBOX(MOD_SEL, PrekinTabs::Onmod_sel)
  EVT_CHECKBOX(MANUAL_INPUT, PrekinTabs::Onmanual_input)
  EVT_RADIOBUTTON(OXIDATION_FLAG1, PrekinTabs::Onoxidation_flag)
  EVT_RADIOBUTTON(OXIDATION_FLAG2, PrekinTabs::Onoxidation_flag)
  EVT_CHECKBOX(MIR, PrekinTabs::OnMIR)
  EVT_RADIOBUTTON(GASIFICATION_FLAG1, PrekinTabs::OnGasification_flag)
  EVT_RADIOBUTTON(GASIFICATION_FLAG2, PrekinTabs::OnGasification_flag)
  EVT_RADIOBOX(FOPL_CH, PrekinTabs::OnFOPL_CH)
  EVT_RADIOBOX(LHK_CH, PrekinTabs::OnLHK_CH)
END_EVENT_TABLE()

//Here is the constructor with passed in pointers
Prekin_UI_Dialog
::Prekin_UI_Dialog
(wxWindow* parent, int id,
  double* mode_burning,
  double* linear_swell,
  double* fuel_carbon,
  double* ash_film,
  double* ash_grain_size,
  double* ash_therm_cond,
  double* size_50,
  double* size_200,
  double* T_f,
  double* pore_radii_macro,
  double* pore_radii_micro,
  double* pore_macroposity,
  double* pore_porosity,
  double* CPD_AB,
  double* CPD_AC,
  double* CPD_AG,
  double* CPD_ACR,
  double* CPD_EB,
  double* CPD_EC,
  double* CPD_EG,
  double* CPD_ECR,
  double* CPD_EBSIG,
  double* CPD_EGSIG,
  double* TS_A1,
  double* TS_A2,
  double* TS_E1,
  double* TS_E2,
  double* TS_Y1,
  double* TS_Y2,
  double* MI_P0,
  double* MI_C0,
  double* MI_SIGP1,
  double* MI_MW,
  double* MDEL,
  double* heat_rate,
  double* max_temp,
  double* res_time,
  double* num_grid_heating,
  double* num_grid_isothermal,
  double* MIR_koso,
  double* MIR_ko,
  double* MIR_so,
  double* MIR_IAE,
  double* MIR_IRO,
  double* MIR_k3o,
  double* MIR_k2ok3o,
  double* MIR_k3ok1o,
  double* MIR_E1,
  double* MIR_E2,
  double* MIR_E3,
  double* IRO_Step2,
  double* Aco,
  double* Eco,
  double* FORL_ko,
  double* FORL_so,
  double* FORL_IAE,
  double* FORL_IRO,
  double* LHK_k1o,
  double* LHK_k2o,
  double* LHK_k3o,
  double* LHK_E1,
  double* LHK_E2,
  double* LHK_E3,
  double* PR_ratio_fr,
  double* PR_ratio_to,
  double* num_steps,
  double* mean_rxn_temp,
  double* mrt_error,
  double* mrt_step,
  double* reac_frac_fr,
  double* reac_frac_to,
  double* reac_pres_step,
  double* total_pres,
  double* time_intv,
  double* time_step,
  double* conv_level,
  double* optim_kG,
  double* optim_EG,
  double* optim_m,
  double* tolerance,
  string* coal_name,
  string* FORL,
  string* LHK,
  long* Pore_Model,
  long* mod_sel,
  long* manual_input,
  long* oxidation_flag,
  long* MIR,
  long* Gasification_flag,
  long* FORL_CH,
  long* LHK_CH,
  long* Schema)
: UIDialog((wxWindow *) parent, id, "Prekin"),
  p_mode_burning(mode_burning),
  p_linear_swell(linear_swell),
  p_fuel_carbon(fuel_carbon),
  p_ash_film(ash_film),
  p_ash_grain_size(ash_grain_size),
  p_ash_therm_cond(ash_therm_cond),
  p_size_50(size_50),
  p_size_200(size_200),
  p_T_f(T_f),
  p_pore_radii_macro(pore_radii_macro),
  p_pore_radii_micro(pore_radii_micro),
  p_pore_macroposity(pore_macroposity),
  p_pore_porosity(pore_porosity),
  p_CPD_AB(CPD_AB),
  p_CPD_AC(CPD_AC),
  p_CPD_AG(CPD_AG),
  p_CPD_ACR(CPD_ACR),
  p_CPD_EB(CPD_EB),
  p_CPD_EC(CPD_EC),
  p_CPD_EG(CPD_EG),
  p_CPD_ECR(CPD_ECR),
  p_CPD_EBSIG(CPD_EBSIG),
  p_CPD_EGSIG(CPD_EGSIG),
  p_TS_A1(TS_A1),
  p_TS_A2(TS_A2),
  p_TS_E1(TS_E1),
  p_TS_E2(TS_E2),
  p_TS_Y1(TS_Y1),
  p_TS_Y2(TS_Y2),
  p_MI_P0(MI_P0),
  p_MI_C0(MI_C0),
  p_MI_SIGP1(MI_SIGP1),
  p_MI_MW(MI_MW),
  p_MDEL(MDEL),
  p_heat_rate(heat_rate),
  p_max_temp(max_temp),
  p_res_time(res_time),
  p_num_grid_heating(num_grid_heating),
  p_num_grid_isothermal(num_grid_isothermal),
  p_MIR_koso(MIR_koso),
  p_MIR_ko(MIR_ko),
  p_MIR_so(MIR_so),
  p_MIR_IAE(MIR_IAE),
  p_MIR_IRO(MIR_IRO),
  p_MIR_k3o(MIR_k3o),
  p_MIR_k2ok3o(MIR_k2ok3o),
  p_MIR_k3ok1o(MIR_k3ok1o),
  p_MIR_E1(MIR_E1),
  p_MIR_E2(MIR_E2),
  p_MIR_E3(MIR_E3),
  p_IRO_Step2(IRO_Step2),
  p_Aco(Aco),
  p_Eco(Eco),
  p_FORL_ko(FORL_ko),
  p_FORL_so(FORL_so),
  p_FORL_IAE(FORL_IAE),
  p_FORL_IRO(FORL_IRO),
  p_LHK_k1o(LHK_k1o),
  p_LHK_k2o(LHK_k2o),
  p_LHK_k3o(LHK_k3o),
  p_LHK_E1(LHK_E1),
  p_LHK_E2(LHK_E2),
  p_LHK_E3(LHK_E3),
  p_PR_ratio_fr(PR_ratio_fr),
  p_PR_ratio_to(PR_ratio_to),
  p_num_steps(num_steps),
  p_mean_rxn_temp(mean_rxn_temp),
  p_mrt_error(mrt_error),
  p_mrt_step(mrt_step),
  p_reac_frac_fr(reac_frac_fr),
  p_reac_frac_to(reac_frac_to),
  p_reac_pres_step(reac_pres_step),
  p_total_pres(total_pres),
  p_time_intv(time_intv),
  p_time_step(time_step),
  p_conv_level(conv_level),
  p_optim_kG(optim_kG),
  p_optim_EG(optim_EG),
  p_optim_m(optim_m),
  p_tolerance(tolerance),
  p_coal_name(coal_name),
  p_FORL(FORL),
  p_LHK(LHK),
  p_Pore_Model(Pore_Model),
  p_mod_sel(mod_sel),
  p_manual_input(manual_input),
  p_oxidation_flag(oxidation_flag),
  p_MIR(MIR),
  p_Gasification_flag(Gasification_flag),
  p_FORL_CH(FORL_CH),
  p_LHK_CH(LHK_CH),
  p_Schema(Schema)
{
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
   
  m_tabs = new PrekinTabs(this);
  m_tabs->CreateInitialPages();
  m_sizerNotebook = new wxNotebookSizer(m_tabs);
  
  wxBoxSizer *ok_row=new wxBoxSizer(wxHORIZONTAL);
  
  
  top_sizer->Add(m_sizerNotebook, 1, wxEXPAND | wxALL, 4);// wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0); //the bottom margin

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  SetSizer(top_sizer);
  top_sizer->Layout();
  //SetAutoLayout(TRUE);
  top_sizer->Fit(this);
}

/////////////////////////////////////////////////////
Prekin_UI_Dialog
::~Prekin_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Prekin_UI_Dialog::TransferDataFromWindow()
{
  entry2double(m_tabs->t_mode_burning,p_mode_burning);
  entry2double(m_tabs->t_linear_swell,p_linear_swell);
  entry2double(m_tabs->t_fuel_carbon,p_fuel_carbon );
  entry2double(m_tabs->t_ash_film,p_ash_film );
  entry2double(m_tabs->t_ash_grain_size,p_ash_grain_size );
  entry2double(m_tabs->t_ash_therm_cond,p_ash_therm_cond );
  entry2double(m_tabs->t_size_50,p_size_50 );
  entry2double(m_tabs->t_size_200,p_size_200 );
  entry2double(m_tabs->t_T_f,p_T_f );
  entry2double(m_tabs->t_pore_radii_macro, p_pore_radii_macro);
  entry2double(m_tabs->t_pore_radii_micro, p_pore_radii_micro);
  entry2double(m_tabs->t_pore_macroposity, p_pore_macroposity);
  entry2double(m_tabs->t_pore_porosity, p_pore_porosity);
  entry2double(m_tabs->t_CPD_AB, p_CPD_AB);
  entry2double(m_tabs->t_CPD_AC, p_CPD_AC);
  entry2double(m_tabs->t_CPD_AG, p_CPD_AG);
  entry2double(m_tabs->t_CPD_ACR, p_CPD_ACR);
  entry2double(m_tabs->t_CPD_EB, p_CPD_EB);
  entry2double(m_tabs->t_CPD_EC, p_CPD_EC);
  entry2double(m_tabs->t_CPD_EG, p_CPD_EG);
  entry2double(m_tabs->t_CPD_ECR, p_CPD_ECR);
  entry2double(m_tabs->t_CPD_EBSIG, p_CPD_EBSIG);
  entry2double(m_tabs->t_CPD_EGSIG, p_CPD_EGSIG);
  entry2double(m_tabs->t_TS_A1, p_TS_A1);
  entry2double(m_tabs->t_TS_A2, p_TS_A2);
  entry2double(m_tabs->t_TS_E1, p_TS_E1);
  entry2double(m_tabs->t_TS_E2, p_TS_E2);
  entry2double(m_tabs->t_TS_Y1, p_TS_Y1);
  entry2double(m_tabs->t_TS_Y2, p_TS_Y2);
  entry2double(m_tabs->t_MI_P0, p_MI_P0);
  entry2double(m_tabs->t_MI_C0, p_MI_C0);
  entry2double(m_tabs->t_MI_SIGP1, p_MI_SIGP1);
  entry2double(m_tabs->t_MI_MW, p_MI_MW);
  entry2double(m_tabs->t_MDEL, p_MDEL);
  entry2double(m_tabs->t_heat_rate, p_heat_rate);
  entry2double(m_tabs->t_max_temp, p_max_temp);
  entry2double(m_tabs->t_res_time, p_res_time);
  entry2double(m_tabs->t_num_grid_heating, p_num_grid_heating);
  entry2double(m_tabs->t_num_grid_isothermal, p_num_grid_isothermal);
  entry2double(m_tabs->t_MIR_koso, p_MIR_koso);
  entry2double(m_tabs->t_MIR_ko, p_MIR_ko);
  entry2double(m_tabs->t_MIR_so, p_MIR_so);
  entry2double(m_tabs->t_MIR_IAE, p_MIR_IAE);
  entry2double(m_tabs->t_MIR_IRO, p_MIR_IRO);
  entry2double(m_tabs->t_MIR_k3o, p_MIR_k3o);
  entry2double(m_tabs->t_MIR_k2ok3o, p_MIR_k2ok3o);
  entry2double(m_tabs->t_MIR_k3ok1o, p_MIR_k3ok1o);
  entry2double(m_tabs->t_MIR_E1, p_MIR_E1);
  entry2double(m_tabs->t_MIR_E2, p_MIR_E2);
  entry2double(m_tabs->t_MIR_E3, p_MIR_E3);
  entry2double(m_tabs->t_IRO_Step2, p_IRO_Step2);
  entry2double(m_tabs->t_Aco, p_Aco);
  entry2double(m_tabs->t_Eco, p_Eco);
  entry2double(m_tabs->t_FOPL_ko, p_FORL_ko);
  entry2double(m_tabs->t_FOPL_so, p_FORL_so);
  entry2double(m_tabs->t_FOPL_IAE, p_FORL_IAE);
  entry2double(m_tabs->t_FOPL_IRO, p_FORL_IRO);
  entry2double(m_tabs->t_LHK_k1o, p_LHK_k1o);
  entry2double(m_tabs->t_LHK_k2o, p_LHK_k2o);
  entry2double(m_tabs->t_LHK_k3o, p_LHK_k3o);
  entry2double(m_tabs->t_LHK_E1, p_LHK_E1);
  entry2double(m_tabs->t_LHK_E2, p_LHK_E2);
  entry2double(m_tabs->t_LHK_E3, p_LHK_E3);
  entry2double(m_tabs->t_PR_ratio_fr, p_PR_ratio_fr);
  entry2double(m_tabs->t_PR_ratio_to, p_PR_ratio_to);
  entry2double(m_tabs->t_num_steps, p_num_steps);
  entry2double(m_tabs->t_mean_rxn_temp, p_mean_rxn_temp);
  entry2double(m_tabs->t_mrt_error, p_mrt_error);
  entry2double(m_tabs->t_mrt_step, p_mrt_step);
  entry2double(m_tabs->t_reac_frac_fr, p_reac_frac_fr);
  entry2double(m_tabs->t_reac_frac_to, p_reac_frac_to);
  entry2double(m_tabs->t_reac_pres_step, p_reac_pres_step);
  entry2double(m_tabs->t_total_pres, p_total_pres);
  entry2double(m_tabs->t_time_intv, p_time_intv);
  entry2double(m_tabs->t_time_step, p_time_step);
  entry2double(m_tabs->t_conv_level, p_conv_level);
  entry2double(m_tabs->t_optim_kG, p_optim_kG);
  entry2double(m_tabs->t_optim_EG, p_optim_EG);
  entry2double(m_tabs->t_optim_m, p_optim_m);
  entry2double(m_tabs->t_tolerance, p_tolerance);

  (*p_coal_name) = m_tabs->cb_coal_name->GetValue();
  (*p_FORL) = m_tabs->cb_FOPL->GetValue();
  (*p_LHK) = m_tabs->cb_LHK->GetValue();

  *p_Pore_Model=m_tabs->rb_Pore_Model1->GetValue();
    
  *p_mod_sel=m_tabs->rb_mod_sel->GetSelection();
  
  *p_manual_input=m_tabs->cbo_manual_input->GetValue();

  *p_oxidation_flag=m_tabs->rb_oxidation_flag1->GetValue();
  
  *p_MIR=m_tabs->cbo_MIR->GetValue();

  *p_Gasification_flag=m_tabs->rb_Gasification_flag1->GetValue();
  
  *p_FORL_CH=m_tabs->rb_FOPL_CH->GetSelection();
  
  *p_LHK_CH=m_tabs->rb_LHK_CH->GetSelection();
  
  *p_Schema=m_tabs->rb_Schema->GetSelection();

  return true;
}

////////////////////////////////////////////////////
bool Prekin_UI_Dialog::TransferDataToWindow()
{
  double2entry(m_tabs->t_mode_burning,p_mode_burning);
  double2entry(m_tabs->t_linear_swell,p_linear_swell);
  double2entry(m_tabs->t_fuel_carbon,p_fuel_carbon );
  double2entry(m_tabs->t_ash_film,p_ash_film );
  double2entry(m_tabs->t_ash_grain_size,p_ash_grain_size );
  double2entry(m_tabs->t_ash_therm_cond,p_ash_therm_cond );
  double2entry(m_tabs->t_size_50,p_size_50 );
  double2entry(m_tabs->t_size_200,p_size_200 );
  double2entry(m_tabs->t_T_f,p_T_f );
  double2entry(m_tabs->t_pore_radii_macro, p_pore_radii_macro);
  double2entry(m_tabs->t_pore_radii_micro, p_pore_radii_micro);
  double2entry(m_tabs->t_pore_macroposity, p_pore_macroposity);
  double2entry(m_tabs->t_pore_porosity, p_pore_porosity);
  double2entry(m_tabs->t_CPD_AB, p_CPD_AB);
  double2entry(m_tabs->t_CPD_AC, p_CPD_AC);
  double2entry(m_tabs->t_CPD_AG, p_CPD_AG);
  double2entry(m_tabs->t_CPD_ACR, p_CPD_ACR);
  double2entry(m_tabs->t_CPD_EB, p_CPD_EB);
  double2entry(m_tabs->t_CPD_EC, p_CPD_EC);
  double2entry(m_tabs->t_CPD_EG, p_CPD_EG);
  double2entry(m_tabs->t_CPD_ECR, p_CPD_ECR);
  double2entry(m_tabs->t_CPD_EBSIG, p_CPD_EBSIG);
  double2entry(m_tabs->t_CPD_EGSIG, p_CPD_EGSIG);
  double2entry(m_tabs->t_TS_A1, p_TS_A1);
  double2entry(m_tabs->t_TS_A2, p_TS_A2);
  double2entry(m_tabs->t_TS_E1, p_TS_E1);
  double2entry(m_tabs->t_TS_E2, p_TS_E2);
  double2entry(m_tabs->t_TS_Y1, p_TS_Y1);
  double2entry(m_tabs->t_TS_Y2, p_TS_Y2);
  double2entry(m_tabs->t_MI_P0, p_MI_P0);
  double2entry(m_tabs->t_MI_C0, p_MI_C0);
  double2entry(m_tabs->t_MI_SIGP1, p_MI_SIGP1);
  double2entry(m_tabs->t_MI_MW, p_MI_MW);
  double2entry(m_tabs->t_MDEL, p_MDEL);
  double2entry(m_tabs->t_heat_rate, p_heat_rate);
  double2entry(m_tabs->t_max_temp, p_max_temp);
  double2entry(m_tabs->t_res_time, p_res_time);
  double2entry(m_tabs->t_num_grid_heating, p_num_grid_heating);
  double2entry(m_tabs->t_num_grid_isothermal, p_num_grid_isothermal);
  double2entry(m_tabs->t_MIR_koso, p_MIR_koso);
  double2entry(m_tabs->t_MIR_ko, p_MIR_ko);
  double2entry(m_tabs->t_MIR_so, p_MIR_so);
  double2entry(m_tabs->t_MIR_IAE, p_MIR_IAE);
  double2entry(m_tabs->t_MIR_IRO, p_MIR_IRO);
  double2entry(m_tabs->t_MIR_k3o, p_MIR_k3o);
  double2entry(m_tabs->t_MIR_k2ok3o, p_MIR_k2ok3o);
  double2entry(m_tabs->t_MIR_k3ok1o, p_MIR_k3ok1o);
  double2entry(m_tabs->t_MIR_E1, p_MIR_E1);
  double2entry(m_tabs->t_MIR_E2, p_MIR_E2);
  double2entry(m_tabs->t_MIR_E3, p_MIR_E3);
  double2entry(m_tabs->t_IRO_Step2, p_IRO_Step2);
  double2entry(m_tabs->t_Aco, p_Aco);
  double2entry(m_tabs->t_Eco, p_Eco);
  double2entry(m_tabs->t_FOPL_ko, p_FORL_ko);
  double2entry(m_tabs->t_FOPL_so, p_FORL_so);
  double2entry(m_tabs->t_FOPL_IAE, p_FORL_IAE);
  double2entry(m_tabs->t_FOPL_IRO, p_FORL_IRO);
  double2entry(m_tabs->t_LHK_k1o, p_LHK_k1o);
  double2entry(m_tabs->t_LHK_k2o, p_LHK_k2o);
  double2entry(m_tabs->t_LHK_k3o, p_LHK_k3o);
  double2entry(m_tabs->t_LHK_E1, p_LHK_E1);
  double2entry(m_tabs->t_LHK_E2, p_LHK_E2);
  double2entry(m_tabs->t_LHK_E3, p_LHK_E3);
  double2entry(m_tabs->t_PR_ratio_fr, p_PR_ratio_fr);
  double2entry(m_tabs->t_PR_ratio_to, p_PR_ratio_to);
  double2entry(m_tabs->t_num_steps, p_num_steps);
  double2entry(m_tabs->t_mean_rxn_temp, p_mean_rxn_temp);
  double2entry(m_tabs->t_mrt_error, p_mrt_error);
  double2entry(m_tabs->t_mrt_step, p_mrt_step);
  double2entry(m_tabs->t_reac_frac_fr, p_reac_frac_fr);
  double2entry(m_tabs->t_reac_frac_to, p_reac_frac_to);
  double2entry(m_tabs->t_reac_pres_step, p_reac_pres_step);
  double2entry(m_tabs->t_total_pres, p_total_pres);
  double2entry(m_tabs->t_time_intv, p_time_intv);
  double2entry(m_tabs->t_time_step, p_time_step);
  double2entry(m_tabs->t_conv_level, p_conv_level);
  double2entry(m_tabs->t_optim_kG, p_optim_kG);
  double2entry(m_tabs->t_optim_EG, p_optim_EG);
  double2entry(m_tabs->t_optim_m, p_optim_m);
  double2entry(m_tabs->t_tolerance, p_tolerance);
  
  m_tabs->cb_coal_name->SetValue(p_coal_name->c_str());
  m_tabs->cb_FOPL->SetValue(p_FORL->c_str());
  m_tabs->cb_LHK->SetValue(p_LHK->c_str());

  m_tabs->rb_Pore_Model1->SetValue(bool(*p_Pore_Model));
  m_tabs->rb_Pore_Model2->SetValue(!bool(*p_Pore_Model));  

  m_tabs->rb_mod_sel->SetSelection(*p_mod_sel);
  
  m_tabs->cbo_manual_input->SetValue(bool(*p_manual_input));

  m_tabs->rb_oxidation_flag1->SetValue(bool(*p_oxidation_flag));
  m_tabs->rb_oxidation_flag2->SetValue(!bool(*p_oxidation_flag));
  
  m_tabs->cbo_MIR->SetValue(bool(*p_MIR));

  m_tabs->rb_Gasification_flag1->SetValue(bool(*p_Gasification_flag));
  m_tabs->rb_Gasification_flag2->SetValue(!bool(*p_Gasification_flag));
  
  m_tabs->rb_FOPL_CH->SetSelection(*p_FORL_CH);
  
  m_tabs->rb_LHK_CH->SetSelection(*p_LHK_CH);
  
  m_tabs->rb_Schema->SetSelection(*p_Schema);

  wxCommandEvent event;

  m_tabs->OnPoreModel(event);
  m_tabs->Onmod_sel(event);
  m_tabs->Onmanual_input(event);
  m_tabs->Onoxidation_flag(event);
  m_tabs->OnMIR(event);
  m_tabs->OnGasification_flag(event);
  m_tabs->OnFOPL_CH(event);
  m_tabs->OnLHK_CH(event);

  return true;
}

void Prekin_UI_Dialog::Lock(bool l)
{
}

void Prekin_UI_Dialog:: double2entry(wxTextCtrl* entry, double * value)
{
  wxString txt;
  txt<<(*value);
  entry->SetValue(txt);
}

void Prekin_UI_Dialog::entry2double(wxTextCtrl* entry, double * value)
{
  wxString txt;
  txt=entry->GetValue();
  (*value) = atof(txt.c_str());
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
PrekinTabs::PrekinTabs(wxWindow *parent, wxWindowID id,
	   const wxPoint& pos , 
	   const wxSize& size , 
	   long style)
  : wxNotebook(parent, id, pos, size, style)
{
}

void PrekinTabs::CreateInitialPages()
{
    wxPanel *panel = (wxPanel *) NULL;

    // Create and add some panels to the notebook

    panel = CreateFirstPage();
    AddPage( panel, _T("Coal"), true);

    panel = CreateSecondPage();
    AddPage( panel, _T("Devolatilization"), false);

	panel = CreateThirdPage();
    AddPage( panel, _T("Oxidation"), false);

	panel = CreateForthPage();
    AddPage( panel, _T("Gasification"), false);

	panel = CreateFifthPage();
    AddPage( panel, _T("Reaction Conditions"), false);
 
}

wxPanel* PrekinTabs::CreateFirstPage()
{
  wxPanel *panel = new wxPanel(this);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  
  wxStaticBox *part_box = new wxStaticBox(panel, -1, "Particle Size Distribution");
  wxStaticBox *pore_box = new wxStaticBox(panel, -1, "Pore Structure Model");
  
  wxBoxSizer* coal_row = new wxBoxSizer(wxVERTICAL);

  wxStaticBoxSizer* part_row = new wxStaticBoxSizer(part_box, wxVERTICAL);
  wxStaticBoxSizer* pore_row = new wxStaticBoxSizer(pore_box, wxVERTICAL);

  top_sizer->Add(10, 5, 0);
  top_sizer->Add(coal_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(part_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(pore_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  
  wxBoxSizer* coal_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_row2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_row3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_row4 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_row5 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_row6 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* coal_row7 = new wxBoxSizer(wxHORIZONTAL);


  coal_row->Add(10, 5, 0);
  coal_row->Add(coal_row1);
  coal_row->Add(10, 5, 0);
  coal_row->Add(coal_row2);
  coal_row->Add(10, 5, 0);
  coal_row->Add(coal_row3);
  coal_row->Add(10, 5, 0);
  coal_row->Add(coal_row4);
  coal_row->Add(10, 5, 0);
  coal_row->Add(coal_row5);
  coal_row->Add(10, 5, 0);
  coal_row->Add(coal_row6);
  coal_row->Add(10, 5, 0);
  coal_row->Add(coal_row7);
  coal_row->Add(10, 5, 0);
  
  

  wxStaticText * label0 = new wxStaticText(panel, -1, "Coal Name", wxDefaultPosition, wxSize(150, 17));

  wxString coal_type_val[] = {
	wxT("Pittsburg_#8"), wxT("Illinois_#5"), wxT("Illinois_#6"), wxT("Petcoke"),
	wxT("Pike_County"), wxT("Pocahantas_#3"), wxT("E-Gas_Illinois_#6"), wxT("E-Gas_Utah"), 
	wxT("E-Gas_Wyodak"), wxT("E-Gas_Wyoming"), wxT("E-Gas_AppMS"), wxT("E-Gas_AppLS")
  };
  
  cb_coal_name = new wxComboBox(panel, -1, wxT("Pittsburg_#8"), wxDefaultPosition, wxSize(150, 20), 12, coal_type_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  coal_row1->Add(label0);
  coal_row1->Add(cb_coal_name);

  wxStaticText * label1 = new wxStaticText(panel, -1, "Mode of Burning", wxDefaultPosition, wxSize(220, 17));
  t_mode_burning = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  coal_row2->Add(label1);
  coal_row2->Add(t_mode_burning);

  wxStaticText * label2 = new wxStaticText(panel, -1, "Linear Swelling Factor", wxDefaultPosition, wxSize(220, 17));
  t_linear_swell = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  coal_row3->Add(label2);
  coal_row3->Add(t_linear_swell);

  wxStaticText * label3 = new wxStaticText(panel, -1, "Initial Fuel Carbon Density, g/cm3", wxDefaultPosition, wxSize(220, 17));
  t_fuel_carbon = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  coal_row4->Add(label3);
  coal_row4->Add(t_fuel_carbon);

  wxStaticText * label4 = new wxStaticText(panel, -1, "Ash Film Porosity", wxDefaultPosition, wxSize(220, 17));
  t_ash_film = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  coal_row5->Add(label4);
  coal_row5->Add(t_ash_film);

  wxStaticText * label5 = new wxStaticText(panel, -1, "Ash Grain Size, um", wxDefaultPosition, wxSize(220, 17));
  t_ash_grain_size = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  coal_row6->Add(label5);
  coal_row6->Add(t_ash_grain_size);

  wxStaticText * label6 = new wxStaticText(panel, -1, "Ash Thermal Conductivity, cal/cm-s-deg", wxDefaultPosition, wxSize(220, 17));
  t_ash_therm_cond = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  coal_row7->Add(label6);
  coal_row7->Add(t_ash_therm_cond);


  wxBoxSizer* part_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* part_row2 = new wxBoxSizer(wxHORIZONTAL);
  
  part_row->Add(10, 5, 0);
  part_row->Add(part_row1);
  part_row->Add(10, 5, 0);
  part_row->Add(part_row2);
  part_row->Add(10, 5, 0);
  
  wxStaticText * label7 = new wxStaticText(panel, -1, " % through 50 Mesh", wxDefaultPosition, wxSize(220, 17));
  t_size_50 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  part_row1->Add(label7);
  part_row1->Add(t_size_50);
  

  wxStaticText * label8 = new wxStaticText(panel, -1, " % through 200 Mesh", wxDefaultPosition, wxSize(220, 17));
  t_size_200 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  part_row2->Add(label8);
  part_row2->Add(t_size_200);
  
  wxBoxSizer* pore_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* pore_row2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* pore_row3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* pore_row4 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* pore_row5 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* pore_row6 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* pore_row7 = new wxBoxSizer(wxHORIZONTAL);
  
  pore_row->Add(10, 5, 0);
  pore_row->Add(pore_row1);
  pore_row->Add(10, 5, 0);
  pore_row->Add(pore_row2);
  pore_row->Add(10, 5, 0);
  pore_row->Add(pore_row3);
  pore_row->Add(10, 5, 0);
  pore_row->Add(pore_row4);
  pore_row->Add(10, 5, 0);
  pore_row->Add(pore_row5);
  pore_row->Add(10, 5, 0);
  pore_row->Add(pore_row6);
  pore_row->Add(10, 5, 0);
  pore_row->Add(pore_row7);
  pore_row->Add(10, 5, 0);
  
  rb_Pore_Model1 = new wxRadioButton(panel, PORE_MODEL1, wxT("Parallel Model"), wxDefaultPosition, wxSize(220, 20),wxRB_GROUP);
  pore_row1->Add(rb_Pore_Model1);
  
  wxStaticText * label9 = new wxStaticText(panel, -1, "   T/f", wxDefaultPosition, wxSize(220, 17));
  t_T_f = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  pore_row2->Add(label9);
  pore_row2->Add(t_T_f);
  
  rb_Pore_Model2 = new wxRadioButton(panel, PORE_MODEL2, wxT("Random Pore Model"), wxDefaultPosition, wxSize(220, 20));
  pore_row3->Add(rb_Pore_Model2);
  
  wxStaticText * label10 = new wxStaticText(panel, -1, "   Average Radii For Macropore, um", wxDefaultPosition, wxSize(220, 17));
  t_pore_radii_macro = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  pore_row4->Add(label10);
  pore_row4->Add(t_pore_radii_macro);

  wxStaticText * label11 = new wxStaticText(panel, -1, "   Average Radii For Micropore, um", wxDefaultPosition, wxSize(220, 17));
  t_pore_radii_micro = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  pore_row5->Add(label11);
  pore_row5->Add(t_pore_radii_micro);
  
  wxStaticText * label12 = new wxStaticText(panel, -1, "   Macroporosity", wxDefaultPosition, wxSize(220, 17));
  t_pore_macroposity = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  pore_row6->Add(label12);
  pore_row6->Add(t_pore_macroposity);

  wxStaticText * label13 = new wxStaticText(panel, -1, "   Porosity", wxDefaultPosition, wxSize(220, 17));
  t_pore_porosity = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  pore_row7->Add(label13);
  pore_row7->Add(t_pore_porosity);

  //top_sizer->Fit(panel);
  //panel->SetAutoLayout(true);
  panel->SetSizer(top_sizer);

  return panel;
}

wxPanel* PrekinTabs::CreateSecondPage()
{
  wxPanel *panel = new wxPanel(this);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  
  wxStaticBox *model_box = new wxStaticBox(panel, -1, "Model Selector");
  wxStaticBox *devol_box = new wxStaticBox(panel, -1, "Devolatilization Conditions");
  
  wxStaticBoxSizer* model_row = new wxStaticBoxSizer(model_box, wxVERTICAL);
  wxStaticBoxSizer* devol_row = new wxStaticBoxSizer(devol_box, wxVERTICAL);

  top_sizer->Add(10, 5, 0);
  top_sizer->Add(model_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(devol_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  
  wxBoxSizer* model_row1 = new wxBoxSizer(wxHORIZONTAL);
  
  wxStaticBox *CPD_box = new wxStaticBox(panel, -1, "CPD parameters");
  wxStaticBox *TwoStep_box = new wxStaticBox(panel, -1, "Two-Step parameters");
  wxStaticBoxSizer* CPD_row = new wxStaticBoxSizer(CPD_box, wxVERTICAL);
  wxStaticBoxSizer* TwoStep_row = new wxStaticBoxSizer(TwoStep_box, wxVERTICAL);

  model_row->Add(10, 5, 0);
  model_row->Add(model_row1);
  model_row->Add(10, 5, 0);
  model_row->Add(CPD_row);
  model_row->Add(10, 5, 0);
  model_row->Add(TwoStep_row);
  model_row->Add(10, 5, 0);

  wxString model_val[] = {
    wxT("Manual Input of Volatile Yield"), wxT("120% of ASTM"), wxT("CPD"), wxT("Two-Step")  
  };
  
  rb_mod_sel = new wxRadioBox(panel, MOD_SEL, wxT("Model"), wxDefaultPosition, wxDefaultSize, 4, model_val, 2);

  model_row1->Add(rb_mod_sel);
  
  wxGridSizer* CPD_grid = new wxGridSizer(4, 6, 3, 5); //4 rows, 6 cols, v gap 5, h gap 10
  wxBoxSizer* MI_row = new wxBoxSizer(wxHORIZONTAL);
  wxGridSizer* MI_grid = new wxGridSizer(2, 6, 3, 5); //2 rows, 6 cols, v gap 5, h gap 10

  CPD_row->Add(10, 5, 0);
  CPD_row->Add(CPD_grid);
  CPD_row->Add(10, 5, 0);
  CPD_row->Add(MI_row);
  CPD_row->Add(10, 5, 0);
  CPD_row->Add(MI_grid);
  CPD_row->Add(10, 5, 0);

  wxStaticText * label0 = new wxStaticText(panel, -1, "AB", wxDefaultPosition, wxSize(55, 17));
  t_CPD_AB = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label0);
  CPD_grid->Add(t_CPD_AB);
  wxStaticText * label1 = new wxStaticText(panel, -1, "EB", wxDefaultPosition, wxSize(55, 17));
  t_CPD_EB = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label1);
  CPD_grid->Add(t_CPD_EB);
  wxStaticText * label2 = new wxStaticText(panel, -1, "EBSIG", wxDefaultPosition, wxSize(55, 17));
  t_CPD_EBSIG = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label2);
  CPD_grid->Add(t_CPD_EBSIG);
  wxStaticText * label3 = new wxStaticText(panel, -1, "AC", wxDefaultPosition, wxSize(55, 17));
  t_CPD_AC = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label3);
  CPD_grid->Add(t_CPD_AC);
  wxStaticText * label4 = new wxStaticText(panel, -1, "EC", wxDefaultPosition, wxSize(55, 17));
  t_CPD_EC = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label4);
  CPD_grid->Add(t_CPD_EC);
  wxStaticText * label5 = new wxStaticText(panel, -1, " ", wxDefaultPosition, wxSize(55, 17)); //place holder
  wxStaticText * label6 = new wxStaticText(panel, -1, " ", wxDefaultPosition, wxSize(55, 17)); //place holder
  CPD_grid->Add(label5);
  CPD_grid->Add(label6);
  wxStaticText * label7 = new wxStaticText(panel, -1, "AG", wxDefaultPosition, wxSize(55, 17));
  t_CPD_AG = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label7);
  CPD_grid->Add(t_CPD_AG);
  wxStaticText * label8 = new wxStaticText(panel, -1, "EG", wxDefaultPosition, wxSize(55, 17));
  t_CPD_EG = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label8);
  CPD_grid->Add(t_CPD_EG);
  wxStaticText * label9 = new wxStaticText(panel, -1, "EGSIG", wxDefaultPosition, wxSize(55, 17));
  t_CPD_EGSIG = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label9);
  CPD_grid->Add(t_CPD_EGSIG);
  wxStaticText * label10 = new wxStaticText(panel, -1, "ACR", wxDefaultPosition, wxSize(55, 17));
  t_CPD_ACR = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label10);
  CPD_grid->Add(t_CPD_ACR);
  wxStaticText * label11 = new wxStaticText(panel, -1, "ECR", wxDefaultPosition, wxSize(55, 17));
  t_CPD_ECR = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  CPD_grid->Add(label11);
  CPD_grid->Add(t_CPD_ECR);

  cbo_manual_input = new wxCheckBox(panel, MANUAL_INPUT, "Structure paremeters : Manual Input");
  MI_row->Add(cbo_manual_input);

  wxStaticText * label12 = new wxStaticText(panel, -1, "P0", wxDefaultPosition, wxSize(55, 17));
  t_MI_P0 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  MI_grid->Add(label12);
  MI_grid->Add(t_MI_P0);
  wxStaticText * label13 = new wxStaticText(panel, -1, "C0", wxDefaultPosition, wxSize(55, 17));
  t_MI_C0 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  MI_grid->Add(label13);
  MI_grid->Add(t_MI_C0);
  wxStaticText * label14 = new wxStaticText(panel, -1, "SIGP1", wxDefaultPosition, wxSize(55, 17));
  t_MI_SIGP1 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  MI_grid->Add(label14);
  MI_grid->Add(t_MI_SIGP1);
  wxStaticText * label15 = new wxStaticText(panel, -1, "MW", wxDefaultPosition, wxSize(55, 17));
  t_MI_MW = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  MI_grid->Add(label15);
  MI_grid->Add(t_MI_MW);
  wxStaticText * label16 = new wxStaticText(panel, -1, "MDEL", wxDefaultPosition, wxSize(55, 17));
  t_MDEL = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  MI_grid->Add(label16);
  MI_grid->Add(t_MDEL);

  //the block for the two parameters
  wxGridSizer* TS_grid = new wxGridSizer(2, 6, 3, 5); //4 rows, 6 cols, v gap 3, h gap 5

  TwoStep_row->Add(10, 5, 0);
  TwoStep_row->Add(TS_grid);
  TwoStep_row->Add(10, 5, 0);

  wxStaticText * label17 = new wxStaticText(panel, -1, "A1,1/s", wxDefaultPosition, wxSize(55, 17));
  t_TS_A1 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  TS_grid->Add(label17);
  TS_grid->Add(t_TS_A1);
  wxStaticText * label18 = new wxStaticText(panel, -1, "E1,cal/mol", wxDefaultPosition, wxSize(55, 17));
  t_TS_E1 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  TS_grid->Add(label18);
  TS_grid->Add(t_TS_E1);
  wxStaticText * label19 = new wxStaticText(panel, -1, "Y1", wxDefaultPosition, wxSize(55, 17));
  t_TS_Y1 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  TS_grid->Add(label19);
  TS_grid->Add(t_TS_Y1);
  wxStaticText * label20 = new wxStaticText(panel, -1, "A2,1/s", wxDefaultPosition, wxSize(55, 17));
  t_TS_A2 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  TS_grid->Add(label20);
  TS_grid->Add(t_TS_A2);
  wxStaticText * label21 = new wxStaticText(panel, -1, "E2,cal/mol", wxDefaultPosition, wxSize(55, 17));
  t_TS_E2 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  TS_grid->Add(label21);
  TS_grid->Add(t_TS_E2);
  wxStaticText * label22 = new wxStaticText(panel, -1, "Y2", wxDefaultPosition, wxSize(55, 17));
  t_TS_Y2 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(55, 20));
  TS_grid->Add(label22);
  TS_grid->Add(t_TS_Y2);

  wxBoxSizer* devol_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* devol_row2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* devol_row3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* devol_row4 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* devol_row5 = new wxBoxSizer(wxHORIZONTAL);
  
  devol_row->Add(10, 5, 0);
  devol_row->Add(devol_row1);
  devol_row->Add(10, 5, 0);
  devol_row->Add(devol_row2);
  devol_row->Add(10, 5, 0);
  devol_row->Add(devol_row3);
  devol_row->Add(10, 5, 0);
  devol_row->Add(devol_row4);
  devol_row->Add(10, 5, 0);
  devol_row->Add(devol_row5);
  devol_row->Add(10, 5, 0);

  wxStaticText * label23 = new wxStaticText(panel, -1, "Heating Rates, deg/s", wxDefaultPosition, wxSize(230, 17));
  t_heat_rate = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(135, 20));
  devol_row1->Add(label23);
  devol_row1->Add(t_heat_rate);
  wxStaticText * label24 = new wxStaticText(panel, -1, "Maximum Temp, K", wxDefaultPosition, wxSize(230, 17));
  t_max_temp = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(135, 20));
  devol_row2->Add(label24);
  devol_row2->Add(t_max_temp);
  wxStaticText * label25 = new wxStaticText(panel, -1, "Total Residence Time, sec", wxDefaultPosition, wxSize(230, 17));
  t_res_time = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(135, 20));
  devol_row3->Add(label25);
  devol_row3->Add(t_res_time);
  wxStaticText * label26 = new wxStaticText(panel, -1, "# of Grid Points: Heating", wxDefaultPosition, wxSize(230, 17));
  t_num_grid_heating = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(135, 20));
  devol_row4->Add(label26);
  devol_row4->Add(t_num_grid_heating);
  wxStaticText * label27 = new wxStaticText(panel, -1, "# of Grid Points: Isothermal", wxDefaultPosition, wxSize(230, 17));
  t_num_grid_isothermal = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(135, 20));
  devol_row5->Add(label27);
  devol_row5->Add(t_num_grid_isothermal);

  //top_sizer->Fit(panel);
  //panel->SetAutoLayout(true);
  panel->SetSizer(top_sizer);

  return panel;  
}

wxPanel* PrekinTabs::CreateThirdPage()
{
  wxPanel *panel = new wxPanel(this);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  
  wxStaticBox *FOPL_box = new wxStaticBox(panel, -1, "FOPL Parameters");
  wxStaticBox *SG_box = new wxStaticBox(panel, -1, "Semi-global Parameters");
  wxStaticBox *Ratio_box = new wxStaticBox(panel, -1, "CO/CO2 Ratio");
  
  wxStaticBoxSizer* FOPL_row = new wxStaticBoxSizer(FOPL_box, wxVERTICAL);
  wxStaticBoxSizer* SG_row = new wxStaticBoxSizer(SG_box, wxVERTICAL);
  wxBoxSizer* rFOPL_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* rSG_row = new wxBoxSizer(wxHORIZONTAL);
  wxStaticBoxSizer* Ratio_row = new wxStaticBoxSizer(Ratio_box, wxHORIZONTAL);

  top_sizer->Add(10, 5, 0);
  top_sizer->Add(rFOPL_row,0, wxALIGN_CENTER_HORIZONTAL); 
  top_sizer->Add(FOPL_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(rSG_row,0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(SG_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 15, 0);
  top_sizer->Add(Ratio_row,0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  rb_oxidation_flag1 = new wxRadioButton(panel, OXIDATION_FLAG1, wxT("Fractional Order Power Law"), wxDefaultPosition, wxSize(300, 20),wxRB_GROUP);
  rFOPL_row->Add(rb_oxidation_flag1);
  rb_oxidation_flag2 = new wxRadioButton(panel, OXIDATION_FLAG2, wxT("Semi-global"), wxDefaultPosition, wxSize(300,20));
  rSG_row->Add(rb_oxidation_flag2);
  
  wxBoxSizer* FOPL_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row4 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row5 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row6 = new wxBoxSizer(wxHORIZONTAL);
  
  FOPL_row->Add(10, 5, 0);
  FOPL_row->Add(FOPL_row1);
  FOPL_row->Add(10, 5, 0);
  FOPL_row->Add(FOPL_row2);
  FOPL_row->Add(10, 5, 0);
  FOPL_row->Add(FOPL_row3);
  FOPL_row->Add(10, 5, 0);
  FOPL_row->Add(FOPL_row4);
  FOPL_row->Add(10, 5, 0);
  FOPL_row->Add(FOPL_row5);
  FOPL_row->Add(10, 5, 0);
  FOPL_row->Add(FOPL_row6);
  FOPL_row->Add(10, 5, 0);
  
  cbo_MIR = new wxCheckBox(panel, MIR, "Mass-specific Intrinsic Reactivity");
  FOPL_row1->Add(cbo_MIR);

  wxStaticText * label0 = new wxStaticText(panel, -1, "    KoSo, gC/gC-s-(mol/m^3)^n", wxDefaultPosition, wxSize(220, 17));
  t_MIR_koso = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row2->Add(label0);
  FOPL_row2->Add(t_MIR_koso);
  wxStaticText * label1 = new wxStaticText(panel, -1, "    Ko, gC/m^2-s-(mol/m^3)^n", wxDefaultPosition, wxSize(220, 17));
  t_MIR_ko = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row3->Add(label1);
  FOPL_row3->Add(t_MIR_ko);
  wxStaticText * label2 = new wxStaticText(panel, -1, "    So, m^2C/gC", wxDefaultPosition, wxSize(220, 17));
  t_MIR_so = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row4->Add(label2);
  FOPL_row4->Add(t_MIR_so);
  wxStaticText * label3 = new wxStaticText(panel, -1, "    Intrinsic Activation Energy,cal/mol", wxDefaultPosition, wxSize(220, 17));
  t_MIR_IAE = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row5->Add(label3);
  FOPL_row5->Add(t_MIR_IAE);
  wxStaticText * label4 = new wxStaticText(panel, -1, "    Intrinsic Reaction Order", wxDefaultPosition, wxSize(220, 17));
  t_MIR_IRO = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row6->Add(label4);
  FOPL_row6->Add(t_MIR_IRO);
  
  wxGridSizer* SG_grid = new wxGridSizer(3, 4, 3, 5); //3 rows, 4 cols, v gap 5, h gap 10
  wxBoxSizer* IRO_row = new wxBoxSizer(wxHORIZONTAL);
  
  SG_row->Add(10, 5, 0);
  SG_row->Add(SG_grid);
  SG_row->Add(10, 5, 0);
  SG_row->Add(IRO_row);
  SG_row->Add(10, 5, 0);

  wxStaticText * label7 = new wxStaticText(panel, -1, "K3o,1/s", wxDefaultPosition, wxSize(70, 17));
  t_MIR_k3o = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  SG_grid->Add(label7);
  SG_grid->Add(t_MIR_k3o);
  wxStaticText * label8 = new wxStaticText(panel, -1, "E1,cal/mol", wxDefaultPosition, wxSize(70, 17));
  t_MIR_E1 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  SG_grid->Add(label8);
  SG_grid->Add(t_MIR_E1);
  wxStaticText * label9 = new wxStaticText(panel, -1, "K2o/K3o", wxDefaultPosition, wxSize(70, 17));
  t_MIR_k2ok3o = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  SG_grid->Add(label9);
  SG_grid->Add(t_MIR_k2ok3o);
  wxStaticText * label10 = new wxStaticText(panel, -1, "E2,cal/mol", wxDefaultPosition, wxSize(70, 17));
  t_MIR_E2 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  SG_grid->Add(label10);
  SG_grid->Add(t_MIR_E2);
  wxStaticText * label11 = new wxStaticText(panel, -1, "K3o/K1o", wxDefaultPosition, wxSize(70, 17));
  t_MIR_k3ok1o = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  SG_grid->Add(label11);
  SG_grid->Add(t_MIR_k3ok1o);
  wxStaticText * label12 = new wxStaticText(panel, -1, "E3,cal/mol", wxDefaultPosition, wxSize(70, 17));
  t_MIR_E3 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  SG_grid->Add(label12);
  SG_grid->Add(t_MIR_E3);

  wxStaticText * label13 = new wxStaticText(panel, -1, "Intrinsic Reaction Order for Step 2", wxDefaultPosition, wxSize(225, 17));
  t_IRO_Step2 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  IRO_row->Add(label13);
  IRO_row->Add(t_IRO_Step2);
  

  wxStaticText * label5 = new wxStaticText(panel, -1, "  Aco", wxDefaultPosition, wxSize(75, 17));
  t_Aco = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(75, 20));
  wxStaticText * label6 = new wxStaticText(panel, -1, "  Eco,cal/mol", wxDefaultPosition, wxSize(75, 17));
  t_Eco = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(75, 20));
  Ratio_row->Add(label5);
  Ratio_row->Add(t_Aco);
  //Ratio_row->Add(10, 5, 0);
  Ratio_row->Add(label6);
  Ratio_row->Add(t_Eco);

  //top_sizer->Fit(panel);
  //panel->SetAutoLayout(true);
  panel->SetSizer(top_sizer);

  return panel;  
}

wxPanel* PrekinTabs::CreateForthPage()
{
  wxPanel *panel = new wxPanel(this);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  
  wxStaticBox *Ratio_box = new wxStaticBox(panel, -1, "Product/Reactant Ratio");
  
  wxBoxSizer* rFOPL_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_block = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* rLHK_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* LHK_block = new wxBoxSizer(wxVERTICAL);
  wxStaticBoxSizer* Ratio_row = new wxStaticBoxSizer(Ratio_box, wxHORIZONTAL);

  top_sizer->Add(10, 5, 0);
  top_sizer->Add(rFOPL_row, 0, wxALIGN_CENTER_HORIZONTAL); 
  top_sizer->Add(FOPL_block, 0, wxBOTTOM|wxALIGN_CENTER_HORIZONTAL, 2);
  top_sizer->Add(10, 10, 0);
  top_sizer->Add(rLHK_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(LHK_block, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0);
  top_sizer->Add(Ratio_row, 0,wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  rb_Gasification_flag1 = new wxRadioButton(panel, GASIFICATION_FLAG1, wxT("Fractional Order Power Law"), wxDefaultPosition, wxSize(220, 20),wxRB_GROUP);
  wxString fopl_val[] = { wxT("PLK1")  };
  cb_FOPL = new wxComboBox(panel, -1, wxT("PLK1"), wxDefaultPosition, wxSize(80, 20), 1, fopl_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  rFOPL_row->Add(rb_Gasification_flag1);
  rFOPL_row->Add(cb_FOPL);

  rb_Gasification_flag2 = new wxRadioButton(panel, GASIFICATION_FLAG2, wxT("Langmuire-Hinshelwood Kinetics"), wxDefaultPosition, wxSize(220, 20));
  wxString lhk_val[] = { wxT("LHK1")  };
  cb_LHK = new wxComboBox(panel, -1, wxT("LHK1"), wxDefaultPosition, wxSize(80, 20), 1, lhk_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  rLHK_row->Add(rb_Gasification_flag2);
  rLHK_row->Add(cb_LHK);

  wxBoxSizer* FOPL_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row4 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* FOPL_row5 = new wxBoxSizer(wxHORIZONTAL);
  
  FOPL_block->Add(FOPL_row1);
  FOPL_block->Add(10, 5, 0);
  FOPL_block->Add(FOPL_row2);
  FOPL_block->Add(10, 5, 0);
  FOPL_block->Add(FOPL_row3);
  FOPL_block->Add(10, 5, 0);
  FOPL_block->Add(FOPL_row4);
  FOPL_block->Add(10, 5, 0);
  FOPL_block->Add(FOPL_row5);
  FOPL_block->Add(10, 5, 0);

  wxString CH_val[] = { wxT("CO2"), wxT("H2O")  };
  
  rb_FOPL_CH = new wxRadioBox(panel, FOPL_CH, wxT(""), wxDefaultPosition, wxDefaultSize, 2, CH_val, 1, wxRA_SPECIFY_ROWS);
  FOPL_row1->Add(rb_FOPL_CH);

  wxStaticText * label0 = new wxStaticText(panel, -1, "Ko,gC/m^2-s-(mol/m^3)^n", wxDefaultPosition, wxSize(220, 17));
  t_FOPL_ko = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row2->Add(label0);
  FOPL_row2->Add(t_FOPL_ko);
  wxStaticText * label1 = new wxStaticText(panel, -1, "So,m^2/gC", wxDefaultPosition, wxSize(220, 17));
  t_FOPL_so = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row3->Add(label1);
  FOPL_row3->Add(t_FOPL_so);
  wxStaticText * label2 = new wxStaticText(panel, -1, "Intrinsic Activation Energy,cal/mol", wxDefaultPosition, wxSize(220, 17));
  t_FOPL_IAE = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row4->Add(label2);
  FOPL_row4->Add(t_FOPL_IAE);
  wxStaticText * label3 = new wxStaticText(panel, -1, "Intrinsic Reaction Order", wxDefaultPosition, wxSize(220, 17));
  t_FOPL_IRO = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  FOPL_row5->Add(label3);
  FOPL_row5->Add(t_FOPL_IRO);

  wxBoxSizer* LHK_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxGridSizer* LHK_grid = new wxGridSizer(3, 4, 3, 5); //3 rows, 4 cols, v gap 5, h gap 10
	  
  LHK_block->Add(LHK_row1);
  LHK_block->Add(10, 5, 0);
  LHK_block->Add(LHK_grid);
  LHK_block->Add(10, 5, 0);
  
  rb_LHK_CH = new wxRadioBox(panel, LHK_CH, wxT(""), wxDefaultPosition, wxDefaultSize, 2, CH_val, 1, wxRA_SPECIFY_ROWS);
  LHK_row1->Add(rb_LHK_CH);

  wxStaticText * label4 = new wxStaticText(panel, -1, "K1o,1/atm-s", wxDefaultPosition, wxSize(70, 17));
  t_LHK_k1o = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  LHK_grid->Add(label4);
  LHK_grid->Add(t_LHK_k1o);
  wxStaticText * label5 = new wxStaticText(panel, -1, "E1,cal/mol", wxDefaultPosition, wxSize(70, 17));
  t_LHK_E1 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  LHK_grid->Add(label5);
  LHK_grid->Add(t_LHK_E1);
  wxStaticText * label6 = new wxStaticText(panel, -1, "K2o,1/atm", wxDefaultPosition, wxSize(70, 17));
  t_LHK_k2o = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  LHK_grid->Add(label6);
  LHK_grid->Add(t_LHK_k2o);
  wxStaticText * label7 = new wxStaticText(panel, -1, "E2,cal/mol", wxDefaultPosition, wxSize(70, 17));
  t_LHK_E2 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  LHK_grid->Add(label7);
  LHK_grid->Add(t_LHK_E2);
  wxStaticText * label8 = new wxStaticText(panel, -1, "K3o,1/atm", wxDefaultPosition, wxSize(70, 17));
  t_LHK_k3o = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  LHK_grid->Add(label8);
  LHK_grid->Add(t_LHK_k3o);
  wxStaticText * label9 = new wxStaticText(panel, -1, "E3", wxDefaultPosition, wxSize(70, 17));
  t_LHK_E3 = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(70, 20));
  LHK_grid->Add(label9);
  LHK_grid->Add(t_LHK_E3);

  wxStaticText * label10 = new wxStaticText(panel, -1, "from", wxDefaultPosition, wxSize(50, 17));
  t_PR_ratio_fr = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(50, 20));
  wxStaticText * label11 = new wxStaticText(panel, -1, "to", wxDefaultPosition, wxSize(50, 17));
  t_PR_ratio_to = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(50, 20));
  wxStaticText * label12 = new wxStaticText(panel, -1, "# of steps", wxDefaultPosition, wxSize(50, 17));
  t_num_steps = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(50, 20));

  Ratio_row->Add(label10);
  Ratio_row->Add(t_PR_ratio_fr);
  Ratio_row->Add(label11);
  Ratio_row->Add(t_PR_ratio_to);
  Ratio_row->Add(label12);
  Ratio_row->Add(t_num_steps);

  //top_sizer->Fit(panel);
  //panel->SetAutoLayout(true);
  panel->SetSizer(top_sizer);

  return panel;  
}

wxPanel* PrekinTabs::CreateFifthPage()
{
  wxPanel *panel = new wxPanel(this);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  
  wxStaticBox *simcon_box = new wxStaticBox(panel, -1, "Simulation Conditions");
  wxStaticBox *optim_box = new wxStaticBox(panel, -1, "Optimization");
   
  wxStaticBoxSizer* simcon_row = new wxStaticBoxSizer(simcon_box, wxVERTICAL);
  wxStaticBoxSizer* optim_row = new wxStaticBoxSizer(optim_box, wxVERTICAL);
  
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(simcon_row, 0, wxALIGN_CENTER_HORIZONTAL); 
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(optim_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  
  wxBoxSizer* simcon_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* simcon_row2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* simcon_row3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* simcon_row4 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* simcon_row5 = new wxBoxSizer(wxHORIZONTAL);
  
  simcon_row->Add(10, 5, 0);
  simcon_row->Add(simcon_row1);
  simcon_row->Add(10, 5, 0);
  simcon_row->Add(simcon_row2);
  simcon_row->Add(10, 5, 0);
  simcon_row->Add(simcon_row3);
  simcon_row->Add(10, 5, 0);
  simcon_row->Add(simcon_row4);
  simcon_row->Add(10, 5, 0);
  simcon_row->Add(simcon_row5);
  simcon_row->Add(10, 5, 0);

  wxStaticText * label0 = new wxStaticText(panel, -1, "Mean Rxn Temp, K", wxDefaultPosition, wxSize(110, 17));
  t_mean_rxn_temp = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize( 40, 20));
  wxStaticText * label1 = new wxStaticText(panel, -1, "+-%", wxDefaultPosition, wxSize(20, 17));
  t_mrt_error = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));
  wxStaticText * label2 = new wxStaticText(panel, -1, " # of Temp Steps", wxDefaultPosition, wxSize(100, 17));
  t_mrt_step = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));

  simcon_row1->Add(label0);
  simcon_row1->Add(t_mean_rxn_temp);
  simcon_row1->Add(label1);
  simcon_row1->Add(t_mrt_error);
  simcon_row1->Add(label2);
  simcon_row1->Add(t_mrt_step);

  wxStaticText * label3 = new wxStaticText(panel, -1, "Reactant Fractions", wxDefaultPosition, wxSize(110, 17));
  t_reac_frac_fr = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));
  wxStaticText * label4 = new wxStaticText(panel, -1, " to", wxDefaultPosition, wxSize(20, 17));
  t_reac_frac_to = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));
  wxStaticText * label5 = new wxStaticText(panel, -1, " # of Pressure Steps", wxDefaultPosition, wxSize(100, 17));
  t_reac_pres_step = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));

  simcon_row2->Add(label3);
  simcon_row2->Add(t_reac_frac_fr);
  simcon_row2->Add(label4);
  simcon_row2->Add(t_reac_frac_to);
  simcon_row2->Add(label5);
  simcon_row2->Add(t_reac_pres_step);

  wxStaticText * label6 = new wxStaticText(panel, -1, "Total Pressure,atm", wxDefaultPosition, wxSize(110, 17));
  t_total_pres = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));
  
  simcon_row3->Add(label6);
  simcon_row3->Add(t_total_pres);

  wxStaticText * label7 = new wxStaticText(panel, -1, "Time Interval,sec", wxDefaultPosition, wxSize(110, 17));
  t_time_intv = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));
  wxStaticText * label8 = new wxStaticText(panel, -1, "", wxDefaultPosition, wxSize(60, 17)); //Place holder
  wxStaticText * label9 = new wxStaticText(panel, -1, " # of Time Steps", wxDefaultPosition, wxSize(100, 17));
  t_time_step = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));

  simcon_row4->Add(label7);
  simcon_row4->Add(t_time_intv);
  simcon_row4->Add(label8);
  simcon_row4->Add(label9);
  simcon_row4->Add(t_time_step);

  wxStaticText * label10 = new wxStaticText(panel, -1, "Conversion Level,%", wxDefaultPosition, wxSize(110, 17));
  t_conv_level = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(40, 20));
  
  simcon_row5->Add(label10);
  simcon_row5->Add(t_conv_level);

  wxStaticBox *sv_box = new wxStaticBox(panel, -1, "Staring Values :");
  wxStaticBoxSizer* sv_row = new wxStaticBoxSizer(sv_box, wxVERTICAL);
  wxBoxSizer* optim_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxString schema_val[] = {
    wxT("Varying kG only"), wxT("Varying kG and m"), wxT("Varying All")
  };
  
  rb_Schema = new wxRadioBox(panel, -1, wxT("Schema:"), wxDefaultPosition, wxDefaultSize, 3, schema_val);
  
  optim_row->Add(10, 5, 0);
  optim_row->Add(sv_row, 0, wxALIGN_CENTER_HORIZONTAL); 
  optim_row->Add(10, 5, 0);
  optim_row->Add(optim_row1, 0, wxALIGN_CENTER_HORIZONTAL);
  optim_row->Add(10, 5, 0);
  optim_row->Add(rb_Schema, 0, wxALIGN_CENTER_HORIZONTAL);
  optim_row->Add(10, 5, 0);
  
  wxBoxSizer* sv_row1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* sv_row2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* sv_row3 = new wxBoxSizer(wxHORIZONTAL);
  
  sv_row->Add(10, 5, 0);
  sv_row->Add(sv_row1);
  sv_row->Add(10, 5, 0);
  sv_row->Add(sv_row2);
  sv_row->Add(10, 5, 0);
  sv_row->Add(sv_row3);
  sv_row->Add(10, 5, 0);
  
  wxStaticText * label11 = new wxStaticText(panel, -1, "kG, kgC/m^2-s-(Pa)^m", wxDefaultPosition, wxSize(260, 17));
  t_optim_kG = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  sv_row1->Add(label11);
  sv_row1->Add(t_optim_kG);

  wxStaticText * label12 = new wxStaticText(panel, -1, "EG, kcal/mol", wxDefaultPosition, wxSize(260, 17));
  t_optim_EG = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  sv_row2->Add(label12);
  sv_row2->Add(t_optim_EG);

  wxStaticText * label13 = new wxStaticText(panel, -1, "m", wxDefaultPosition, wxSize(260, 17));
  t_optim_m = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  sv_row3->Add(label13);
  sv_row3->Add(t_optim_m);

  wxStaticText * label14 = new wxStaticText(panel, -1, "Tolerance", wxDefaultPosition, wxSize(260, 17));
  t_tolerance = new wxTextCtrl(panel, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  optim_row1->Add(label14);
  optim_row1->Add(t_tolerance);

  //top_sizer->Fit(panel);
  //panel->SetAutoLayout(true);
  panel->SetSizer(top_sizer);

  return panel;  
}

void PrekinTabs::OnPoreModel(wxCommandEvent &event)
{
	if (rb_Pore_Model1->GetValue()==TRUE)
	{
		t_T_f->Enable(true);
		t_pore_radii_macro->Enable(false);
		t_pore_radii_micro->Enable(false);
		t_pore_macroposity->Enable(false);
		t_pore_porosity->Enable(false);
	}
	else
	{
		t_T_f->Enable(false);
		t_pore_radii_macro->Enable(true);
		t_pore_radii_micro->Enable(true);
		t_pore_macroposity->Enable(true);
		t_pore_porosity->Enable(true);
	}
}

void PrekinTabs::Onmod_sel(wxCommandEvent &event)
{
	int v =rb_mod_sel->GetSelection(); 
	
	if (v==2) //CPD
	{
		 t_CPD_AB->Enable(true);
		 t_CPD_AC->Enable(true);
		 t_CPD_AG->Enable(true);
		 t_CPD_ACR->Enable(true);
		 t_CPD_EB->Enable(true);
		 t_CPD_EC->Enable(true);
		 t_CPD_EG->Enable(true);
		 t_CPD_ECR->Enable(true);
		 t_CPD_EBSIG->Enable(true);
		 t_CPD_EGSIG->Enable(true);
		 t_TS_A1->Enable(false);
		 t_TS_A2->Enable(false);
		 t_TS_E1->Enable(false);
		 t_TS_E2->Enable(false);
		 t_TS_Y1->Enable(false);
		 t_TS_Y2->Enable(false);
		 cbo_manual_input->Enable(true);
		 Onmanual_input(event);
	}
	else if (v==3) //Two Step
	{
		t_CPD_AB->Enable(false);
		 t_CPD_AC->Enable(false);
		 t_CPD_AG->Enable(false);
		 t_CPD_ACR->Enable(false);
		 t_CPD_EB->Enable(false);
		 t_CPD_EC->Enable(false);
		 t_CPD_EG->Enable(false);
		 t_CPD_ECR->Enable(false);
		 t_CPD_EBSIG->Enable(false);
		 t_CPD_EGSIG->Enable(false);
		 t_TS_A1->Enable(true);
		 t_TS_A2->Enable(true);
		 t_TS_E1->Enable(true);
		 t_TS_E2->Enable(true);
		 t_TS_Y1->Enable(true);
		 t_TS_Y2->Enable(true);
		 cbo_manual_input->Enable(false);
		 t_MI_P0->Enable(false);
		 t_MI_C0->Enable(false);
		 t_MI_SIGP1->Enable(false);
		 t_MI_MW->Enable(false);
		 t_MDEL->Enable(false);
	}
	else
	{
		t_CPD_AB->Enable(false);
		 t_CPD_AC->Enable(false);
		 t_CPD_AG->Enable(false);
		 t_CPD_ACR->Enable(false);
		 t_CPD_EB->Enable(false);
		 t_CPD_EC->Enable(false);
		 t_CPD_EG->Enable(false);
		 t_CPD_ECR->Enable(false);
		 t_CPD_EBSIG->Enable(false);
		 t_CPD_EGSIG->Enable(false);
		 t_TS_A1->Enable(false);
		 t_TS_A2->Enable(false);
		 t_TS_E1->Enable(false);
		 t_TS_E2->Enable(false);
		 t_TS_Y1->Enable(false);
		 t_TS_Y2->Enable(false);
		 cbo_manual_input->Enable(false);
		 t_MI_P0->Enable(false);
		 t_MI_C0->Enable(false);
		 t_MI_SIGP1->Enable(false);
		 t_MI_MW->Enable(false);
		 t_MDEL->Enable(false);
	}
}

void PrekinTabs::Onmanual_input(wxCommandEvent &event)
{
	if (cbo_manual_input->GetValue())
	{
		 t_MI_P0->Enable(true);
		 t_MI_C0->Enable(true);
		 t_MI_SIGP1->Enable(true);
		 t_MI_MW->Enable(true);
		 t_MDEL->Enable(true);
	}
	else
	{
		t_MI_P0->Enable(false);
		 t_MI_C0->Enable(false);
		 t_MI_SIGP1->Enable(false);
		 t_MI_MW->Enable(false);
		 t_MDEL->Enable(false);
	}
}

void PrekinTabs::Onoxidation_flag(wxCommandEvent &event)
{
	if (rb_oxidation_flag1->GetValue())
	{
		 t_MIR_k3o->Enable(false);
		 t_MIR_k2ok3o->Enable(false);
		 t_MIR_k3ok1o->Enable(false);
		 t_MIR_E1->Enable(false);
		 t_MIR_E2->Enable(false);
		 t_MIR_E3->Enable(false);
		 t_IRO_Step2->Enable(false);
		 cbo_MIR->Enable(true);
		 OnMIR(event);
	}
	else
	{
		 t_MIR_k3o->Enable(true);
		 t_MIR_k2ok3o->Enable(true);
		 t_MIR_k3ok1o->Enable(true);
		 t_MIR_E1->Enable(true);
		 t_MIR_E2->Enable(true);
		 t_MIR_E3->Enable(true);
		 t_IRO_Step2->Enable(true);
		 cbo_MIR->Enable(false);
		t_MIR_koso->Enable(false);
		t_MIR_ko->Enable(false);
		t_MIR_so->Enable(false);
		t_MIR_IAE->Enable(false);
		t_MIR_IRO->Enable(false);
  	}
}
  
void PrekinTabs::OnMIR(wxCommandEvent &event)
{
	if (cbo_MIR->GetValue())
	{
		t_MIR_koso->Enable(true);
		t_MIR_ko->Enable(true);
		t_MIR_so->Enable(true);
		t_MIR_IAE->Enable(true);
		t_MIR_IRO->Enable(true);
	}
	else
	{
		t_MIR_koso->Enable(false);
		t_MIR_ko->Enable(false);
		t_MIR_so->Enable(false);
		t_MIR_IAE->Enable(false);
		t_MIR_IRO->Enable(false);
	}
		 
}

void PrekinTabs::OnGasification_flag(wxCommandEvent &event)
{
	if (rb_Gasification_flag1->GetValue())
	{
		 t_FOPL_ko->Enable(true);
		 t_FOPL_so->Enable(true);
		 t_FOPL_IAE->Enable(true);
		 t_FOPL_IRO->Enable(true);
		 rb_FOPL_CH->Enable(true);
		 t_LHK_k1o->Enable(false);
		 t_LHK_k2o->Enable(false);
		 t_LHK_k3o->Enable(false);
		 t_LHK_E1->Enable(false);
		 t_LHK_E2->Enable(false);
		 t_LHK_E3->Enable(false);
		 rb_LHK_CH->Enable(false);
	}
	else
	{
		 t_FOPL_ko->Enable(false);
		 t_FOPL_so->Enable(false);
		 t_FOPL_IAE->Enable(false);
		 t_FOPL_IRO->Enable(false);
		 rb_FOPL_CH->Enable(false);
		 t_LHK_k1o->Enable(true);
		 t_LHK_k2o->Enable(true);
		 t_LHK_k3o->Enable(true);
		 t_LHK_E1->Enable(true);
		 t_LHK_E2->Enable(true);
		 t_LHK_E3->Enable(true);
		 rb_LHK_CH->Enable(true);
	}
}

void PrekinTabs::OnFOPL_CH(wxCommandEvent &event)
{

}
  
void PrekinTabs::OnLHK_CH(wxCommandEvent &event)
{

}

