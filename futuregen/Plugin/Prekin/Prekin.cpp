
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Prekin.h"
#include "Prekin_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Prekin, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Prekin
::Prekin()
{
  RegistVar("mode_burning", &mode_burning);
  RegistVar("linear_swell", &linear_swell);
  RegistVar("fuel_carbon", &fuel_carbon);
  RegistVar("ash_film", &ash_film);
  RegistVar("ash_grain_size", &ash_grain_size);
  RegistVar("ash_therm_cond", &ash_therm_cond);
  RegistVar("size_50", &size_50);
  RegistVar("size_200", &size_200);
  RegistVar("T_f", &T_f);
  RegistVar("pore_radii_macro", &pore_radii_macro);
  RegistVar("pore_radii_micro", &pore_radii_micro);
  RegistVar("pore_macroposity", &pore_macroposity);
  RegistVar("pore_porosity", &pore_porosity);
  RegistVar("CPD_AB", &CPD_AB);
  RegistVar("CPD_AC", &CPD_AC);
  RegistVar("CPD_AG", &CPD_AG);
  RegistVar("CPD_ACR", &CPD_ACR);
  RegistVar("CPD_EB", &CPD_EB);
  RegistVar("CPD_EC", &CPD_EC);
  RegistVar("CPD_EG", &CPD_EG);
  RegistVar("CPD_ECR", &CPD_ECR);
  RegistVar("CPD_EBSIG", &CPD_EBSIG);
  RegistVar("CPD_EGSIG", &CPD_EGSIG);
  RegistVar("TS_A1", &TS_A1);
  RegistVar("TS_A2", &TS_A2);
  RegistVar("TS_E1", &TS_E1);
  RegistVar("TS_E2", &TS_E2);
  RegistVar("TS_Y1", &TS_Y1);
  RegistVar("TS_Y2", &TS_Y2);
  RegistVar("MI_P0", &MI_P0);
  RegistVar("MI_C0", &MI_C0);
  RegistVar("MI_SIGP1", &MI_SIGP1);
  RegistVar("MI_MW", &MI_MW);
  RegistVar("MDEL", &MDEL);
  RegistVar("heat_rate", &heat_rate);
  RegistVar("max_temp", &max_temp);
  RegistVar("res_time", &res_time);
  RegistVar("num_grid_heating", &num_grid_heating);
  RegistVar("num_grid_isothermal", &num_grid_isothermal);
  RegistVar("MIR_koso", &MIR_koso);
  RegistVar("MIR_ko", &MIR_ko);
  RegistVar("MIR_so", &MIR_so);
  RegistVar("MIR_IAE", &MIR_IAE);
  RegistVar("MIR_IRo", &MIR_IRo);
  RegistVar("MIR_k3o", &MIR_k3o);
  RegistVar("MIR_k2ok3o", &MIR_k2ok3o);
  RegistVar("MIR_k3ok1o", &MIR_k3ok1o);
  RegistVar("MIR_E1", &MIR_E1);
  RegistVar("MIR_E2", &MIR_E2);
  RegistVar("MIR_E3", &MIR_E3);
  RegistVar("IRO_Step2", &IRO_Step2);
  RegistVar("Aco", &Aco);
  RegistVar("Eco", &Eco);
  RegistVar("FORL_ko", &FORL_ko);
  RegistVar("FORL_so", &FORL_so);
  RegistVar("FORL_IAE", &FORL_IAE);
  RegistVar("FORL_IRO", &FORL_IRO);
  RegistVar("LHK_k1o", &LHK_k1o);
  RegistVar("LHK_k2o", &LHK_k2o);
  RegistVar("LHK_k3o", &LHK_k3o);
  RegistVar("LHK_E1", &LHK_E1);
  RegistVar("LHK_E2", &LHK_E2);
  RegistVar("LHK_E3", &LHK_E3);
  RegistVar("PR_ratio_fr", &PR_ratio_fr);
  RegistVar("PR_ratio_to", &PR_ratio_to);
  RegistVar("num_steps", &num_steps);
  RegistVar("mean_rxn_temp", &mean_rxn_temp);
  RegistVar("mrt_error", &mrt_error);
  RegistVar("mrt_step", &mrt_step);
  RegistVar("reac_frac_fr", &reac_frac_fr);
  RegistVar("reac_frac_to", &reac_frac_to);
  RegistVar("reac_pres_step", &reac_pres_step);
  RegistVar("total_pres", &total_pres);
  RegistVar("time_intv", &time_intv);
  RegistVar("time_step", &time_step);
  RegistVar("conv_level", &conv_level);
  RegistVar("optim_kG", &optim_kG);
  RegistVar("optim_EG", &optim_EG);
  RegistVar("optim_m", &optim_m);
  RegistVar("tolerance", &tolerance);
  RegistVar("coal_name", &coal_name);
  RegistVar("FORL", &FORL);
  RegistVar("LHK", &LHK);
  RegistVar("Pore_Model", &Pore_Model);
  RegistVar("mod_sel", &mod_sel);
  RegistVar("manual_input", &manual_input);
  RegistVar("oxidation_flag", &oxidation_flag);
  RegistVar("MIR", &MIR);
  RegistVar("Gasification_flag", &Gasification_flag);
  RegistVar("FORL_CH", &FORL_CH);
  RegistVar("LHK_CH", &LHK_CH);
  RegistVar("Schema", &Schema);

  mode_burning = 0.2;
  linear_swell = -1;
  fuel_carbon = -1;
  ash_film = 0.17;
  ash_grain_size = 5,0;
  ash_therm_cond = 0.005;
  size_50 = 97.4;
  size_200 = 73.4;
  T_f = 6.0;
  pore_radii_macro = 75.0;
  pore_radii_micro = 0.1;
  pore_macroposity = 0.1;
  pore_porosity = 0.58;
  CPD_AB = 2.602E+15;
  CPD_AC = 0.9;
  CPD_AG = 3.0E+15;
  CPD_ACR = 3.0E+15;
  CPD_EB = 55400;
  CPD_EC = 0.0;
  CPD_EG = 69000;
  CPD_ECR = 65000;
  CPD_EBSIG = 1800;
  CPD_EGSIG = 8100;
  TS_A1 = 9.62E+5;
  TS_A2 = 1.5E+13;
  TS_E1 = 1.7579E+4;
  TS_E2 = 5.995E+4;
  TS_Y1 = 0.65;
  TS_Y2 = 1.0;
  MI_P0 = 0.61;
  MI_C0 = 0.0;
  MI_SIGP1 = 4.6;
  MI_MW = 267;
  MDEL = 29;
  heat_rate = 1.0E+5;
  max_temp = 2000;
  res_time = 0.1;
  num_grid_heating = 30;
  num_grid_isothermal = 20;
  MIR_koso = -1;
  MIR_ko = 1E+7;
  MIR_so = 500;
  MIR_IAE = 42000;
  MIR_IRo = 0.5;
  MIR_k3o = -1;
  MIR_k2ok3o = 50000;
  MIR_k3ok1o = 1E-6;
  MIR_E1 = 32000;
  MIR_E2 = 28000;
  MIR_E3 = 6000;
  IRO_Step2 = 1.0;
  Aco = 200;
  Eco = 9000;
  FORL_ko = 3.243E+5;
  FORL_so = 200;
  FORL_IAE = 41850;
  FORL_IRO = 1.0;
  LHK_k1o = 5.0E+3;
  LHK_k2o = 1.55E-9;
  LHK_k3o = 1.12E+1;
  LHK_E1 = 36782;
  LHK_E2 = -49966.6;
  LHK_E3 = 7046;
  PR_ratio_fr = 0.01;
  PR_ratio_to = 200;
  num_steps = 20;
  mean_rxn_temp = 1900;
  mrt_error = 15;
  mrt_step = 6;
  reac_frac_fr = 0.02;
  reac_frac_to = 0.99;
  reac_pres_step = 6;
  total_pres = 27.22;
  time_intv = 0.5;
  time_step = 100000;
  conv_level = 90;
  optim_kG = 0.01;
  optim_EG = 41.5;
  optim_m = 0.8;
  tolerance = 1E-30;
  coal_name = "Pittsburg_#8";
  FORL = "PLK1";
  LHK = "LHK1";
  Pore_Model = 0;
  mod_sel = 0;
  manual_input = 0;
  oxidation_flag = 0;
  MIR = 0;
  Gasification_flag = 0;
  FORL_CH = 0;
  LHK_CH = 0;
  Schema = 0;

  wxString icon_file="Icons/recuperator.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
}



/////////////////////////////////////////////////////////////////////////////
Prekin
::~Prekin()
{

}

/////////////////////////////////////////////////////////////////////////////
double Prekin::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Prekin::GetNumPoly()
{
  int result=4;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void Prekin::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int Prekin::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Prekin::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Prekin::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Prekin::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void Prekin::DrawIcon(wxDC* dc)
{
  //Your implementation
	  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Prekin::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Prekin_UI_Dialog(parent, -1,
     &mode_burning,
     &linear_swell,
     &fuel_carbon,
     &ash_film,
     &ash_grain_size,
     &ash_therm_cond,
     &size_50,
     &size_200,
     &T_f,
     &pore_radii_macro,
     &pore_radii_micro,
     &pore_macroposity,
     &pore_porosity,
     &CPD_AB,
     &CPD_AC,
     &CPD_AG,
     &CPD_ACR,
     &CPD_EB,
     &CPD_EC,
     &CPD_EG,
     &CPD_ECR,
     &CPD_EBSIG,
     &CPD_EGSIG,
     &TS_A1,
     &TS_A2,
     &TS_E1,
     &TS_E2,
     &TS_Y1,
     &TS_Y2,
     &MI_P0,
     &MI_C0,
     &MI_SIGP1,
     &MI_MW,
     &MDEL,
     &heat_rate,
     &max_temp,
     &res_time,
     &num_grid_heating,
     &num_grid_isothermal,
     &MIR_koso,
     &MIR_ko,
     &MIR_so,
	 &MIR_IAE,
     &MIR_IRo,
     &MIR_k3o,
     &MIR_k2ok3o,
     &MIR_k3ok1o,
     &MIR_E1,
     &MIR_E2,
     &MIR_E3,
     &IRO_Step2,
     &Aco,
     &Eco,
     &FORL_ko,
     &FORL_so,
     &FORL_IAE,
     &FORL_IRO,
     &LHK_k1o,
     &LHK_k2o,
     &LHK_k3o,
     &LHK_E1,
     &LHK_E2,
     &LHK_E3,
     &PR_ratio_fr,
     &PR_ratio_to,
     &num_steps,
     &mean_rxn_temp,
     &mrt_error,
     &mrt_step,
     &reac_frac_fr,
     &reac_frac_to,
     &reac_pres_step,
     &total_pres,
     &time_intv,
     &time_step,
     &conv_level,
     &optim_kG,
     &optim_EG,
     &optim_m,
     &tolerance,
     &coal_name,
     &FORL,
     &LHK,
     &Pore_Model,
     &mod_sel,
     &manual_input,
     &oxidation_flag,
     &MIR,
     &Gasification_flag,
     &FORL_CH,
     &LHK_CH,
     &Schema);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Prekin::GetName()
{
  wxString result="REI_Components_Prekin"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Prekin::GetDesc()
{
  wxString result="Prekin module by REI"; //your description

  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Prekin::GetHelp()
{
  wxString result="Framework/index.html"; //your description

  return result;
}

