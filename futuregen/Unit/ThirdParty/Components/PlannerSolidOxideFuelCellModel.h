// PlannerSolidOxideFuelCellModel.h: interface for the CPlannerSolidOxideFuelCellModel class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(_PLANNERSOLIDOXIDEFUELCELLMODEL_)
#define _PLANNERSOLIDOXIDEFUELCELLMODEL_

#include "SolidOxideAnodeGasModel.h"
#include "SolidOxideCathodeGasModel.h"
#include "SolidOxideElectrolyte.h"
#include "PlannerFuelCellModel.h"

namespace Vision21 {


struct REIInput {

  // Anode
  double a_height;
  double a_width;
  double a_space;
  double a_ecd;
  double a_tcoeff;
  double a_thick;
  double a_vel;
  double a_temp;
  double a_ipres;
  double a_opres;
  double a_specie[7];

  // Cathode
  double c_height;
  double c_width;
  double c_space;
  double c_ecdb;
  double c_ecdm;
  double c_tcoeffb;
  double c_tcoeffm;
  double c_thick;
  double c_vel;
  double c_temp;
  double c_ipres;
  double c_opres;
  double c_specie[6];
  
  // Separator
  double s_thick;
  double s_heatcap;
  double s_density;
  
  // Electrolyte
  double e_thick;
  double e_preexp;
  double e_exp;
  
  // Electrodes
  double f_preexp;
  double f_exp;
  double a_preexp;
  double a_exp;
  
  // Interconect
  double i_preexp;
  double i_exp;
  
  // Cells
  double l_heatcap;
  double l_density;
  double l_length;
  double l_width;
  
  // Operational
  bool const_cp;
  bool radiation;
  bool qs_gas;
  bool qs_solid;
  int ax_nodes;
  double loadres;

};

class CPlanarSolidOxideFuelCellModel  : public CPlanarFuelCellModel
{
public:
	CPlanarSolidOxideFuelCellModel();
	virtual ~CPlanarSolidOxideFuelCellModel();

	void SetREIInputs(REIInput *rei_inp);

	class CSolidOxideAnodeGasModel		AnodeGas;
	class CSolidOxideCathodeGasModel	CathodeGas;
	class CSolidOxideElectrolyte		Electrolyte;

};

} // end namespace Vision21

#endif // !defined(_PLANNERSOLIDOXIDEFUELCELLMODEL_)
