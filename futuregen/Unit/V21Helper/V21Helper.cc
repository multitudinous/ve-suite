
#include "V21Helper.h"

///////////////////////////////////////////////////////////////////////////////////////

V21Helper::V21Helper (const char* therm_path)  
{
  //  std::string therm_path = "/home/maguire/futuregen/V21Helper/Therm/therm";
  thermo_database = new thermo(therm_path);
}

///////////////////////////////////////////////////////////////////////////////////////

V21Helper::~V21Helper ()
{
  delete thermo_database;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::IntToGas (Interface *it, Gas &gs)
{
  unsigned int i;
  bool ok = true;

  gs.clear();

  // Gas
  
  gs.CoalCal = it->getDouble("COALCAL", &ok);
  gs.AshCal  = it->getDouble("ASHCAL",  &ok);
  gs.AshpH   = it->getDouble("ASHPH",   &ok);

  gs.pressure_drop = it->getDouble("PRESSURE_DROP", &ok);
 
  gs.thermo_database = thermo_database;

  // GasCell

  gs.gas_composite.area = it->getDouble("AREA", &ok);
  gs.gas_composite.eff  = it->getDouble("EFF",  &ok);
  gs.gas_composite.eta  = it->getDouble("ETA",  &ok);
  gs.gas_composite.chi  = it->getDouble("CHI",  &ok);

  gs.gas_composite.T = it->getDouble("TEMPERATURE", &ok);
  gs.gas_composite.P = it->getDouble("PRESSURE",    &ok);
  gs.gas_composite.M = it->getDouble("FLOWRATE",    &ok);
  if(!ok) ok = true; // ERROR - MISSING A GAS DEFINITION

  gs.gas_composite.tar  = it->getDouble("TAR", &ok);
  gs.gas_composite.soot = it->getDouble("SOOT", &ok);

  gs.gas_composite.mean_size     = it->getDouble("MEAN_SIZE",     &ok);
  gs.gas_composite.size_variance = it->getDouble("SIZE_VARIANCE", &ok);
  gs.gas_composite.T_particle    = it->getDouble("T_PARTICLE",    &ok);
  gs.gas_composite.M_particle    = it->getDouble("M_PARTICLE",    &ok);

  // Compositions

  std::vector<std::string> comp_name = it->getString1D("COMP_NAME", &ok);
  std::vector<double>      comp_conc = it->getDouble1D("COMP_CONC", &ok);
  if(!ok) ok = true; // ERROR - NO GAS COMPOSITION

  std::vector<std::string> part_name = it->getString1D("PART_NAME", &ok);
  std::vector<double>      part_conc = it->getDouble1D("PART_CONC", &ok);
  if(!ok) ok = true; // ERROR - NO PARTICLE COMPOSITION
  
  std::vector<std::string> wic_name = it->getString1D("WIC_NAME", &ok);
  std::vector<double>      wic_conc = it->getDouble1D("WIC_CONC", &ok);
  if(!ok) ok = true; // ERROR - NO WIC COMPOSITION

  for(i=0; i<comp_name.size(); i++)
    gs.addSpecie(comp_name[i], comp_conc[i]);

  for(i=0; i<part_name.size(); i++)
    gs.addParticle(part_name[i], part_conc[i]);

  for(i=0; i<wic_name.size(); i++)
    gs.addWic(wic_name[i], wic_conc[i]);

  // Done

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::GasToInt (Gas *gs, Interface &it)
{
  it.clear();

  // Gas

  it.setDouble("COALCAL", gs->CoalCal);
  it.setDouble("ASHCAL",  gs->AshCal);
  it.setDouble("ASHPH",   gs->AshpH);

  it.setDouble("PRESSURE_DROP", gs->pressure_drop);

  // GasCell

  it.setDouble("AREA", gs->gas_composite.area);
  it.setDouble("EFF",  gs->gas_composite.eff);
  it.setDouble("ETA",  gs->gas_composite.eta);
  it.setDouble("CHI",  gs->gas_composite.chi);

  it.setDouble("TEMPERATURE", gs->gas_composite.T);
  it.setDouble("PRESSURE",    gs->gas_composite.P);
  it.setDouble("FLOWRATE",    gs->gas_composite.M);

  it.setDouble("TAR",  gs->gas_composite.tar);
  it.setDouble("SOOT", gs->gas_composite.soot);

  it.setDouble("MEAN_SIZE",     gs->gas_composite.mean_size);
  it.setDouble("SIZE_VARIANCE", gs->gas_composite.size_variance);
  it.setDouble("T_PARTICLE",    gs->gas_composite.T_particle);
  it.setDouble("M_PARTICLE",    gs->gas_composite.M_particle);

  // Compositions

  std::map<std::string, int>::iterator iter;

  std::vector<std::string> name;
  std::vector<double>      conc;

  for(iter=gs->specie.begin(); iter!=gs->specie.end(); iter++) {
    name.push_back(iter->first);
    conc.push_back(gs->gas_composite.comp_specie[iter->second]);
  }
  it.setString1D ("COMP_NAME", name); name.clear();
  it.setDouble1D ("COMP_CONC", conc); conc.clear();

  for(iter=gs->particle.begin(); iter!=gs->particle.end(); iter++) {
    name.push_back(iter->first);
    conc.push_back(gs->gas_composite.comp_particle[iter->second]);
  }
  it.setString1D ("PART_NAME", name); name.clear();
  it.setDouble1D ("PART_CONC", conc); conc.clear();

  for(iter=gs->wics.begin(); iter!=gs->wics.end(); iter++) {
    name.push_back(iter->first);
    conc.push_back(gs->comp_wics[iter->second]);
  }
  it.setString1D ("WIC_NAME", name); name.clear();
  it.setDouble1D ("WIC_CONC", conc); conc.clear();

  // Done

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::IntToSum (Interface *it, summary_values &sv)
{
  sv.clear();

  // FILL IN SV - WHEN IS THIS NEEDED ANYWAY?

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::SumToInt (summary_values* sv, Interface &it)
{
  unsigned int i, num_sum;
  std::string desc,val;

  it.clear();

  for(i=0; i<num_sum; i++)
    it.setString(sv->get_description(i), sv->get_value(i));

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////
