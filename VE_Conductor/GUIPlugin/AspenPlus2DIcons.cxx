#include "VE_Conductor/GUIPlugin/AspenPlus2DIcons.h"

#include <map>
#include <string>

#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_ECN_HS_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_ECN_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_ECN_HT_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_ECN_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_FORCED_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_FORCED_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_INDUCE_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_INDUCE_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_NATURA_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_NATURA_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Analyzer_Analyzer_ANALYZE2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Analyzer_Analyzer_ANALYZER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Analyzer_Analyzer_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/BatchFrac_BatchFrac_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/BatchFrac_BatchFrac_COLUMN1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/BatchFrac_BatchFrac_COLUMN2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/BatchFrac_BatchFrac_COLUMN3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/CCD_CCD_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/CCD_CCD_ICON.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/CFuge_CFuge_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/CFuge_CFuge_ICON.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/ClChng_ClChng_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/ClChng_ClChng_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Compr_Compr_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Compr_Compr_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Compr_Compr_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Compr_Compr_ICON3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crusher_Crusher_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crusher_Crusher_ICON.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crystallizer_Crystallizer_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crystallizer_Crystallizer_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crystallizer_Crystallizer_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Cyclone_Cyclone_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Cyclone_Cyclone_ICON.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Decanter_Decanter_V_DRUM.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Decanter_Decanter_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Decanter_Decanter_H_DRUM.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Distl_Distl_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Distl_Distl_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Distl_Distl_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dryer_Dryer_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dryer_Dryer_FLUIDBED.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dryer_Dryer_FLUIDBED2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dryer_Dryer_SPRAY.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/DSTWU_DSTWU_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/DSTWU_DSTWU_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/DSTWU_DSTWU_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dupl_Dupl_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dupl_Dupl_DOT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dupl_Dupl_HEAT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dupl_Dupl_WORK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/ESP_ESP_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/ESP_ESP_ICON.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Extract_Extract_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Extract_Extract_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Extract_Extract_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Extract_Extract_POD.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FabFl_FabFl_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FabFl_FabFl_ICON.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Feedbl_Feedbl_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Feedbl_Feedbl_FEEDBL.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Filter_Filter_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Filter_Filter_PLATE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Filter_Filter_ROTARY.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_FURNACE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_H_DRUM.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_HEATER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_V_DRUM1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_V_DRUM2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_FURNACE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_H_DRUM.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_HEATER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_V_DRUM1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_V_DRUM2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_3WAY.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_DOT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_HEAT_TEE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_HEAT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_TEE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_TRIANGLE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_WORK_TEE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_WORK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_AIRCOOLER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_COMPR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_FURNACE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_Heater.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_PUMP.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_VALVE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_VALVE2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_VALVE4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HS_1CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HS_1CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HT_1CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HT_1CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_ECN_HS_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_ECN_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_ECN_HT_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_ECN_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HS_2CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HS_2CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HS_4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HT_2CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HT_2CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HT_4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_FORCED_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_FORCED_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_G_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_G_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_GEN_HS.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_GEN_HT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_H_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_H_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_INDUCE_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_INDUCE_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J12_HS1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J12_HS2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J12_HT1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J12_HT2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J21_HS1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J21_HS2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J21_HT1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J21_HT2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_K_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_NATURA_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_NATURA_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_SIMP_HS.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_SIMP_HT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_X_HS_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_X_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_X_HT_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_X_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HS_1CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HS_1CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HT_1CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HT_1CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HS_2CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HS_2CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HS_4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HT_2CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HT_2CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HT_4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_G_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_G_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_GEN_HS.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_GEN_HT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_H_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_H_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J12_HS1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J12_HS2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J12_HT1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J12_HT2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J21_HS1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J21_HS2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J21_HT1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J21_HT2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_K_HT_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_K_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_SIMP_HS.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_SMP_HT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_X_HS_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_X_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_X_HT_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_X_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hierarchy_Hierarchy_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HS_1CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HS_1CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HT_1CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HT_1CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HS_2CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HS_2CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HS_4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HT_2CN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HT_2CO.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HT_4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_G_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_G_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_GEN_HS.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_GEN_HT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_H_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_H_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J12_HS1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J12_HS2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J12_HT1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J12_HT2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J21_HS1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J21_HS2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J21_HT1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J21_HT2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_K_HT_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_K_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_SIMP_HS.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_SMP_HT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_X_HS_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_X_HS_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_X_HT_1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_X_HT_2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HXFlux_HXFlux_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HyCyc_HyCyc_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HyCyc_HyCyc_ICON.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MCompr_MCompr_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MCompr_MCompr_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_ACONTLR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_AINDICTR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_FCONTLR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_FINDICTR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_LCONTLR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_LINDICTR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE5.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE6.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE7.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE8.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_PCONTLR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_PINDICTR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_TCONTLR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_TINDICTR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_BLOCK2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_BLOCK3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_CIRC_MHX.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_COCURNT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_COUNTER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_COUNTER2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_SIMP_MHX.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_3WAY.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_DOT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_HEAT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_HOPPER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_SCREW.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_TANK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_TEE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_TRIANGLE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_VALVE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_WORK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mult_Mult_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mult_Mult_DOT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mult_Mult_HEAT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mult_Mult_WORK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_AIRCOL.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU5.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU6.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU7.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU8.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU9.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_PETLYUK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_PFRAC.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_PREFLASH.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_VACUUM1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_VACUUM2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_ABSBR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU10.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU10F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU11.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU11F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU12.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU12F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU13.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU13F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU14.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU14F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU15.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU15F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU1F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU2F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU3F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU4F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU5.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU5F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU6.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU6F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU7.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU7F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU8.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU8F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU9.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU9F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_FCC_MF1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_FCC_MF2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_FRACT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PFRAC.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PFRACF.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PREFL1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PREFL1F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PREFL2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PREFL2F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_STRIP.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_VACUUM1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_VACUUM1F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_VACUUM2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_VACUUM2F.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_D_PIPE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_HI_PIPE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_U_PIPE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_V_PIPE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_D_PIPE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_HI_PIPE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_U_PIPE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_V_PIPE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pump_Pump_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pump_Pump_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pump_Pump_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Qtvec_Qtvec_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Qtvec_Qtvec_DOT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_ABSBR1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_ABSBR2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_ABSBR3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_DECANT1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_DECANT2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_DECANT3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_FRACT1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_FRACT2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKABS.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKCOL1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKCOL2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKSTR1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKSTR2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_RECT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_STRIP1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_STRIP2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_ABSBR2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_ABSBR3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_ABSORBER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_FRACT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_PACKABS.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_PACKCOL.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_PACKSTR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_RECT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_STRIPPER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_VACUUM1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_VACUUM2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RBatch_RBatch_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RBatch_RBatch_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RCSTR_RCSTR_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RCSTR_RCSTR_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/REquil_REquil_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/REquil_REquil_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/REquil_REquil_ICON3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RGibbs_RGibbs_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RGibbs_RGibbs_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RGibbs_RGibbs_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RPlug_RPlug_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RPlug_RPlug_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RPlug_RPlug_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RPlug_RPlug_ICON3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_ICON3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_ICON4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RYield_RYield_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RYield_RYield_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RYield_RYield_ICON3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_CDU1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_CDU2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_CDU3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_VACUUM1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_VACUUM2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Screen_Screen_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Screen_Screen_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Screen_Screen_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Selector_Selector_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Selector_Selector_HEAT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Selector_Selector_TRIANGLE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Selector_Selector_WORK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep2_Sep2_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep2_Sep2_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep2_Sep2_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep2_Sep2_ICON3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep_Sep_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep_Sep_ICON1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep_Sep_ICON2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep_Sep_ICON3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_3WAY.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_CCD.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_CFUGE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_CYCLONE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_DOT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_FILTER1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_FILTER2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_SCREEN.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_TEE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_TRIANGLE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_VSCRUB.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SWash_SWash_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SWash_SWash_ICON.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_CFUGE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_CSTR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_EXCEL.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_FILTER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_FRACT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_H_DRUM.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_HEATER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_HEATX1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_HEATX2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_PLUG.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_REACTOR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_RECT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_STRIP.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_V_DRUM.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_VALVE4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_CFUGE.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_CSTR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_FILTER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_FRACT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_H_DRUM.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_HEATER.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_HEATX1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_HEATX2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_PLUG.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_REACTOR.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_RECT.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_STRIP.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_V_DRUM.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_VALVE4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User_User_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User_User_SMALL.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_VALVE1.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_VALVE2.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_VALVE3.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_VALVE4.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/VScrub_VScrub_BLOCK.xpm"
#include "VE_Conductor/xpm/AspenPlus2DIcons/VScrub_VScrub_ICON.xpm"

std::map< std::string, char** > GetAspenPlusIconMap()
{
    std::map< std::string, char** > tempIconMap;

    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.BLOCK.jpg" ] = Aerotran_Aerotran_BLOCK;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.ECN-HS-1.jpg" ] = Aerotran_Aerotran_ECN_HS_1;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.ECN-HS-2.jpg" ] = Aerotran_Aerotran_ECN_HS_2;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.ECN-HT-1.jpg" ] = Aerotran_Aerotran_ECN_HT_1;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.ECN-HT-2.jpg" ] = Aerotran_Aerotran_ECN_HT_2;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.FORCED-1.jpg" ] = Aerotran_Aerotran_FORCED_1;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.FORCED-2.jpg" ] = Aerotran_Aerotran_FORCED_2;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.INDUCE-1.jpg" ] = Aerotran_Aerotran_INDUCE_1;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.INDUCE-2.jpg" ] = Aerotran_Aerotran_INDUCE_2;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.NATURA-1.jpg" ] = Aerotran_Aerotran_NATURA_1;
    tempIconMap["2DIcons/AEROTRAN/AEROTRAN.NATURA-2.jpg" ] = Aerotran_Aerotran_NATURA_2;
    tempIconMap["2DIcons/Analyzer/Analyzer.ANALYZE2.jpg" ] = Analyzer_Analyzer_ANALYZE2;
    tempIconMap["2DIcons/Analyzer/Analyzer.ANALYZER.jpg" ] = Analyzer_Analyzer_ANALYZER;
    tempIconMap["2DIcons/Analyzer/Analyzer.BLOCK.jpg" ] = Analyzer_Analyzer_BLOCK;
    tempIconMap["2DIcons/BatchFrac/BatchFrac.BLOCK.jpg" ] = BatchFrac_BatchFrac_BLOCK;
    tempIconMap["2DIcons/BatchFrac/BatchFrac.COLUMN1.jpg" ] = BatchFrac_BatchFrac_COLUMN1;
    tempIconMap["2DIcons/BatchFrac/BatchFrac.COLUMN2.jpg" ] = BatchFrac_BatchFrac_COLUMN2;
    tempIconMap["2DIcons/BatchFrac/BatchFrac.COLUMN3.jpg" ] = BatchFrac_BatchFrac_COLUMN3;
    tempIconMap["2DIcons/CCD/CCD.BLOCK.jpg" ] = CCD_CCD_BLOCK;
    tempIconMap["2DIcons/CCD/CCD.ICON.jpg" ] = CCD_CCD_ICON;
    tempIconMap["2DIcons/CFuge/CFuge.BLOCK.jpg" ] = CFuge_CFuge_BLOCK;
    tempIconMap["2DIcons/CFuge/CFuge.ICON.jpg" ] = CFuge_CFuge_ICON;
    tempIconMap["2DIcons/ClChng/ClChng.BLOCK.jpg" ] = ClChng_ClChng_BLOCK;
    tempIconMap["2DIcons/ClChng/ClChng.ICON1.jpg" ] = ClChng_ClChng_ICON1;
    tempIconMap["2DIcons/Compr/Compr.BLOCK.jpg" ] = Compr_Compr_BLOCK;
    tempIconMap["2DIcons/Compr/Compr.ICON1.jpg" ] = Compr_Compr_ICON1;
    tempIconMap["2DIcons/Compr/Compr.ICON2.jpg" ] = Compr_Compr_ICON2;
    tempIconMap["2DIcons/Compr/Compr.ICON3.jpg" ] = Compr_Compr_ICON3;
    tempIconMap["2DIcons/Crusher/Crusher.BLOCK.jpg" ] = Crusher_Crusher_BLOCK;
    tempIconMap["2DIcons/Crusher/Crusher.ICON.jpg" ] = Crusher_Crusher_ICON;
    tempIconMap["2DIcons/Crystallizer/Crystallizer.BLOCK.jpg" ] = Crystallizer_Crystallizer_BLOCK;
    tempIconMap["2DIcons/Crystallizer/Crystallizer.ICON1.jpg" ] = Crystallizer_Crystallizer_ICON1;
    tempIconMap["2DIcons/Crystallizer/Crystallizer.ICON2.jpg" ] = Crystallizer_Crystallizer_ICON2;
    tempIconMap["2DIcons/Cyclone/Cyclone.BLOCK.jpg" ] = Cyclone_Cyclone_BLOCK;
    tempIconMap["2DIcons/Cyclone/Cyclone.ICON.jpg" ] = Cyclone_Cyclone_ICON;
    tempIconMap["2DIcons/Decanter/Decanter-V-DRUM.jpg" ] = Decanter_Decanter_V_DRUM;
    tempIconMap["2DIcons/Decanter/Decanter.BLOCK.jpg" ] = Decanter_Decanter_BLOCK;
    tempIconMap["2DIcons/Decanter/Decanter.H-DRUM.jpg" ] = Decanter_Decanter_H_DRUM;
    tempIconMap["2DIcons/Distl/Distl.BLOCK.jpg" ] = Distl_Distl_BLOCK;
    tempIconMap["2DIcons/Distl/Distl.ICON1.jpg" ] = Distl_Distl_ICON1;
    tempIconMap["2DIcons/Distl/Distl.ICON2.jpg" ] = Distl_Distl_ICON2;
    tempIconMap["2DIcons/Dryer/Dryer.BLOCK.jpg" ] = Dryer_Dryer_BLOCK;
    tempIconMap["2DIcons/Dryer/Dryer.FLUIDBED.jpg" ] = Dryer_Dryer_FLUIDBED;
    tempIconMap["2DIcons/Dryer/Dryer.FLUIDBED2.jpg" ] = Dryer_Dryer_FLUIDBED2;
    tempIconMap["2DIcons/Dryer/Dryer.SPRAY.jpg" ] = Dryer_Dryer_SPRAY;
    tempIconMap["2DIcons/DSTWU/DSTWU.BLOCK.jpg" ] = DSTWU_DSTWU_BLOCK;
    tempIconMap["2DIcons/DSTWU/DSTWU.ICON1.jpg" ] = DSTWU_DSTWU_ICON1;
    tempIconMap["2DIcons/DSTWU/DSTWU.ICON2.jpg" ] = DSTWU_DSTWU_ICON2;
    tempIconMap["2DIcons/Dupl/Dupl.BLOCK.jpg" ] = Dupl_Dupl_BLOCK;
    tempIconMap["2DIcons/Dupl/Dupl.DOT.jpg" ] = Dupl_Dupl_DOT;
    tempIconMap["2DIcons/Dupl/Dupl.HEAT.jpg" ] = Dupl_Dupl_HEAT;
    tempIconMap["2DIcons/Dupl/Dupl.WORK.jpg" ] = Dupl_Dupl_WORK;
    tempIconMap["2DIcons/ESP/ESP.BLOCK.jpg" ] = ESP_ESP_BLOCK;
    tempIconMap["2DIcons/ESP/ESP.ICON.jpg" ] = ESP_ESP_ICON;
    tempIconMap["2DIcons/Extract/Extract.BLOCK.jpg" ] = Extract_Extract_BLOCK;
    tempIconMap["2DIcons/Extract/Extract.ICON1.jpg" ] = Extract_Extract_ICON1;
    tempIconMap["2DIcons/Extract/Extract.ICON2.jpg" ] = Extract_Extract_ICON2;
    tempIconMap["2DIcons/Extract/Extract.POD.jpg" ] = Extract_Extract_POD;
    tempIconMap["2DIcons/FabFl/FabFl.BLOCK.jpg" ] = FabFl_FabFl_BLOCK;
    tempIconMap["2DIcons/FabFl/FabFl.ICON.jpg" ] = FabFl_FabFl_ICON;
    tempIconMap["2DIcons/Feedbl/Feedbl.BLOCK.jpg" ] = Feedbl_Feedbl_BLOCK;
    tempIconMap["2DIcons/Feedbl/Feedbl.FEEDBL.jpg" ] = Feedbl_Feedbl_FEEDBL;
    tempIconMap["2DIcons/Filter/Filter.BLOCK.jpg" ] = Filter_Filter_BLOCK;
    tempIconMap["2DIcons/Filter/Filter.PLATE.jpg" ] = Filter_Filter_PLATE;
    tempIconMap["2DIcons/Filter/Filter.ROTARY.jpg" ] = Filter_Filter_ROTARY;
    tempIconMap["2DIcons/Flash2/Flash2.BLOCK.jpg" ] = Flash2_Flash2_BLOCK;
    tempIconMap["2DIcons/Flash2/Flash2.FURNACE.jpg" ] = Flash2_Flash2_FURNACE;
    tempIconMap["2DIcons/Flash2/Flash2.H-DRUM.jpg" ] = Flash2_Flash2_H_DRUM;
    tempIconMap["2DIcons/Flash2/Flash2.HEATER.jpg" ] = Flash2_Flash2_HEATER;
    tempIconMap["2DIcons/Flash2/Flash2.V-DRUM1.jpg" ] = Flash2_Flash2_V_DRUM1;
    tempIconMap["2DIcons/Flash2/Flash2.V-DRUM2.jpg" ] = Flash2_Flash2_V_DRUM2;
    tempIconMap["2DIcons/Flash3/Flash3.BLOCK.jpg" ] = Flash3_Flash3_BLOCK;
    tempIconMap["2DIcons/Flash3/Flash3.FURNACE.jpg" ] = Flash3_Flash3_FURNACE;
    tempIconMap["2DIcons/Flash3/Flash3.H-DRUM.jpg" ] = Flash3_Flash3_H_DRUM;
    tempIconMap["2DIcons/Flash3/Flash3.HEATER.jpg" ] = Flash3_Flash3_HEATER;
    tempIconMap["2DIcons/Flash3/Flash3.V-DRUM1.jpg" ] = Flash3_Flash3_V_DRUM1;
    tempIconMap["2DIcons/Flash3/Flash3.V-DRUM2.jpg" ] = Flash3_Flash3_V_DRUM2;
    tempIconMap["2DIcons/FSplit/FSplit.3WAY.jpg" ] = FSplit_FSplit_3WAY;
    tempIconMap["2DIcons/FSplit/FSplit.BLOCK.jpg" ] = FSplit_FSplit_BLOCK;
    tempIconMap["2DIcons/FSplit/FSplit.DOT.jpg" ] = FSplit_FSplit_DOT;
    tempIconMap["2DIcons/FSplit/FSplit.HEAT-TEE.jpg" ] = FSplit_FSplit_HEAT_TEE;
    tempIconMap["2DIcons/FSplit/FSplit.HEAT.jpg" ] = FSplit_FSplit_HEAT;
    tempIconMap["2DIcons/FSplit/FSplit.TEE.jpg" ] = FSplit_FSplit_TEE;
    tempIconMap["2DIcons/FSplit/FSplit.TRIANGLE.jpg" ] = FSplit_FSplit_TRIANGLE;
    tempIconMap["2DIcons/FSplit/FSplit.WORK-TEE.jpg" ] = FSplit_FSplit_WORK_TEE;
    tempIconMap["2DIcons/FSplit/FSplit.WORK.jpg" ] = FSplit_FSplit_WORK;
    tempIconMap["2DIcons/HEATER/HEATER.AIRCOOLER.jpg" ] = Heater_Heater_AIRCOOLER;
    tempIconMap["2DIcons/HEATER/HEATER.BLOCK.jpg" ] = Heater_Heater_BLOCK;
    tempIconMap["2DIcons/HEATER/HEATER.COMPR.jpg" ] = Heater_Heater_COMPR;
    tempIconMap["2DIcons/HEATER/HEATER.FURNACE.jpg" ] = Heater_Heater_FURNACE;
    tempIconMap["2DIcons/HEATER/HEATER.Heater.jpg" ] = Heater_Heater_Heater;
    tempIconMap["2DIcons/HEATER/HEATER.PUMP.jpg" ] = Heater_Heater_PUMP;
    tempIconMap["2DIcons/HEATER/HEATER.VALVE.jpg" ] = Heater_Heater_VALVE;
    tempIconMap["2DIcons/HEATER/HEATER.VALVE2.jpg" ] = Heater_Heater_VALVE2;
    tempIconMap["2DIcons/HEATER/HEATER.VALVE4.jpg" ] = Heater_Heater_VALVE4;
    tempIconMap["2DIcons/HEATX/HEATX.BLOCK.jpg" ] = HeatX_HeatX_BLOCK;
    tempIconMap["2DIcons/HEATX/HEATX.E-HS-1CN.jpg" ] = HeatX_HeatX_E_HS_1CN;
    tempIconMap["2DIcons/HEATX/HEATX.E-HS-1CO.jpg" ] = HeatX_HeatX_E_HS_1CO;
    tempIconMap["2DIcons/HEATX/HEATX.E-HS-2.jpg" ] = HeatX_HeatX_E_HS_2;
    tempIconMap["2DIcons/HEATX/HEATX.E-HT-1CN.jpg" ] = HeatX_HeatX_E_HT_1CN;
    tempIconMap["2DIcons/HEATX/HEATX.E-HT-1CO.jpg" ] = HeatX_HeatX_E_HT_1CO;
    tempIconMap["2DIcons/HEATX/HEATX.E-HT-2.jpg" ] = HeatX_HeatX_E_HT_2;
    tempIconMap["2DIcons/HEATX/HEATX.ECN-HS-1.jpg" ] = HeatX_HeatX_ECN_HS_1;
    tempIconMap["2DIcons/HEATX/HEATX.ECN-HS-2.jpg" ] = HeatX_HeatX_ECN_HS_2;
    tempIconMap["2DIcons/HEATX/HEATX.ECN-HT-1.jpg" ] = HeatX_HeatX_ECN_HT_1;
    tempIconMap["2DIcons/HEATX/HEATX.ECN-HT-2.jpg" ] = HeatX_HeatX_ECN_HT_2;
    tempIconMap["2DIcons/HEATX/HEATX.F-HS-2CN.jpg" ] = HeatX_HeatX_F_HS_2CN;
    tempIconMap["2DIcons/HEATX/HEATX.F-HS-2CO.jpg" ] = HeatX_HeatX_F_HS_2CO;
    tempIconMap["2DIcons/HEATX/HEATX.F-HS-4.jpg" ] = HeatX_HeatX_F_HS_4;
    tempIconMap["2DIcons/HEATX/HEATX.F-HT-2CN.jpg" ] = HeatX_HeatX_F_HT_2CN;
    tempIconMap["2DIcons/HEATX/HEATX.F-HT-2CO.jpg" ] = HeatX_HeatX_F_HT_2CO;
    tempIconMap["2DIcons/HEATX/HEATX.F-HT-4.jpg" ] = HeatX_HeatX_F_HT_4;
    tempIconMap["2DIcons/HEATX/HEATX.FORCED-1.jpg" ] = HeatX_HeatX_FORCED_1;
    tempIconMap["2DIcons/HEATX/HEATX.FORCED-2.jpg" ] = HeatX_HeatX_FORCED_2;
    tempIconMap["2DIcons/HEATX/HEATX.G-HS-2.jpg" ] = HeatX_HeatX_G_HS_2;
    tempIconMap["2DIcons/HEATX/HEATX.G-HT-2.jpg" ] = HeatX_HeatX_G_HT_2;
    tempIconMap["2DIcons/HEATX/HEATX.GEN-HS.jpg" ] = HeatX_HeatX_GEN_HS;
    tempIconMap["2DIcons/HEATX/HEATX.GEN-HT.jpg" ] = HeatX_HeatX_GEN_HT;
    tempIconMap["2DIcons/HEATX/HEATX.H-HS-2.jpg" ] = HeatX_HeatX_H_HS_2;
    tempIconMap["2DIcons/HEATX/HEATX.H-HT-2.jpg" ] = HeatX_HeatX_H_HT_2;
    tempIconMap["2DIcons/HEATX/HEATX.INDUCE-1.jpg" ] = HeatX_HeatX_INDUCE_1;
    tempIconMap["2DIcons/HEATX/HEATX.INDUCE-2.jpg" ] = HeatX_HeatX_INDUCE_2;
    tempIconMap["2DIcons/HEATX/HEATX.J12-HS1.jpg" ] = HeatX_HeatX_J12_HS1;
    tempIconMap["2DIcons/HEATX/HEATX.J12-HS2.jpg" ] = HeatX_HeatX_J12_HS2;
    tempIconMap["2DIcons/HEATX/HEATX.J12-HT1.jpg" ] = HeatX_HeatX_J12_HT1;
    tempIconMap["2DIcons/HEATX/HEATX.J12-HT2.jpg" ] = HeatX_HeatX_J12_HT2;
    tempIconMap["2DIcons/HEATX/HEATX.J21-HS1.jpg" ] = HeatX_HeatX_J21_HS1;
    tempIconMap["2DIcons/HEATX/HEATX.J21-HS2.jpg" ] = HeatX_HeatX_J21_HS2;
    tempIconMap["2DIcons/HEATX/HEATX.J21-HT1.jpg" ] = HeatX_HeatX_J21_HT1;
    tempIconMap["2DIcons/HEATX/HEATX.J21-HT2.jpg" ] = HeatX_HeatX_J21_HT2;
    tempIconMap["2DIcons/HEATX/HEATX.K-HT-2.jpg" ] = HeatX_HeatX_K_HT_2;
    tempIconMap["2DIcons/HEATX/HEATX.NATURA-1.jpg" ] = HeatX_HeatX_NATURA_1;
    tempIconMap["2DIcons/HEATX/HEATX.NATURA-2.jpg" ] = HeatX_HeatX_NATURA_2;
    tempIconMap["2DIcons/HEATX/HEATX.SIMP-HS.jpg" ] = HeatX_HeatX_SIMP_HS;
    tempIconMap["2DIcons/HEATX/HEATX.SIMP-HT.jpg" ] = HeatX_HeatX_SIMP_HT;
    tempIconMap["2DIcons/HEATX/HEATX.X-HS-1.jpg" ] = HeatX_HeatX_X_HS_1;
    tempIconMap["2DIcons/HEATX/HEATX.X-HS-2.jpg" ] = HeatX_HeatX_X_HS_2;
    tempIconMap["2DIcons/HEATX/HEATX.X-HT-1.jpg" ] = HeatX_HeatX_X_HT_1;
    tempIconMap["2DIcons/HEATX/HEATX.X-HT-2.jpg" ] = HeatX_HeatX_X_HT_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.BLOCK.jpg" ] = Hetran_Hetran_BLOCK;
    tempIconMap["2DIcons/HETRAN/HETRAN.E-HS-1CN.jpg" ] = Hetran_Hetran_E_HS_1CN;
    tempIconMap["2DIcons/HETRAN/HETRAN.E-HS-1CO.jpg" ] = Hetran_Hetran_E_HS_1CO;
    tempIconMap["2DIcons/HETRAN/HETRAN.E-HS-2.jpg" ] = Hetran_Hetran_E_HS_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.E-HT-1CN.jpg" ] = Hetran_Hetran_E_HT_1CN;
    tempIconMap["2DIcons/HETRAN/HETRAN.E-HT-1CO.jpg" ] = Hetran_Hetran_E_HT_1CO;
    tempIconMap["2DIcons/HETRAN/HETRAN.E-HT-2.jpg" ] = Hetran_Hetran_E_HT_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.F-HS-2CN.jpg" ] = Hetran_Hetran_F_HS_2CN;
    tempIconMap["2DIcons/HETRAN/HETRAN.F-HS-2CO.jpg" ] = Hetran_Hetran_F_HS_2CO;
    tempIconMap["2DIcons/HETRAN/HETRAN.F-HS-4.jpg" ] = Hetran_Hetran_F_HS_4;
    tempIconMap["2DIcons/HETRAN/HETRAN.F-HT-2CN.jpg" ] = Hetran_Hetran_F_HT_2CN;
    tempIconMap["2DIcons/HETRAN/HETRAN.F-HT-2CO.jpg" ] = Hetran_Hetran_F_HT_2CO;
    tempIconMap["2DIcons/HETRAN/HETRAN.F-HT-4.jpg" ] = Hetran_Hetran_F_HT_4;
    tempIconMap["2DIcons/HETRAN/HETRAN.G-HS-2.jpg" ] = Hetran_Hetran_G_HS_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.G-HT-2.jpg" ] = Hetran_Hetran_G_HT_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.GEN-HS.jpg" ] = Hetran_Hetran_GEN_HS;
    tempIconMap["2DIcons/HETRAN/HETRAN.GEN-HT.jpg" ] = Hetran_Hetran_GEN_HT;
    tempIconMap["2DIcons/HETRAN/HETRAN.H-HS-2.jpg" ] = Hetran_Hetran_H_HS_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.H-HT-2.jpg" ] = Hetran_Hetran_H_HT_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.J12-HS1.jpg" ] = Hetran_Hetran_J12_HS1;
    tempIconMap["2DIcons/HETRAN/HETRAN.J12-HS2.jpg" ] = Hetran_Hetran_J12_HS2;
    tempIconMap["2DIcons/HETRAN/HETRAN.J12-HT1.jpg" ] = Hetran_Hetran_J12_HT1;
    tempIconMap["2DIcons/HETRAN/HETRAN.J12-HT2.jpg" ] = Hetran_Hetran_J12_HT2;
    tempIconMap["2DIcons/HETRAN/HETRAN.J21-HS1.jpg" ] = Hetran_Hetran_J21_HS1;
    tempIconMap["2DIcons/HETRAN/HETRAN.J21-HS2.jpg" ] = Hetran_Hetran_J21_HS2;
    tempIconMap["2DIcons/HETRAN/HETRAN.J21-HT1.jpg" ] = Hetran_Hetran_J21_HT1;
    tempIconMap["2DIcons/HETRAN/HETRAN.J21-HT2.jpg" ] = Hetran_Hetran_J21_HT2;
    tempIconMap["2DIcons/HETRAN/HETRAN.K-HT-1.jpg" ] = Hetran_Hetran_K_HT_1;
    tempIconMap["2DIcons/HETRAN/HETRAN.K-HT-2.jpg" ] = Hetran_Hetran_K_HT_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.SIMP-HS.jpg" ] = Hetran_Hetran_SIMP_HS;
    tempIconMap["2DIcons/HETRAN/HETRAN.SMP-HT.jpg" ] = Hetran_Hetran_SMP_HT;
    tempIconMap["2DIcons/HETRAN/HETRAN.X-HS-1.jpg" ] = Hetran_Hetran_X_HS_1;
    tempIconMap["2DIcons/HETRAN/HETRAN.X-HS-2.jpg" ] = Hetran_Hetran_X_HS_2;
    tempIconMap["2DIcons/HETRAN/HETRAN.X-HT-1.jpg" ] = Hetran_Hetran_X_HT_1;
    tempIconMap["2DIcons/HETRAN/HETRAN.X-HT-2.jpg" ] = Hetran_Hetran_X_HT_2;
    tempIconMap["2DIcons/HIERARCHY/HIERARCHY.BLOCK.jpg" ] = Hierarchy_Hierarchy_BLOCK;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.BLOCK.jpg" ] = HTRIXIST_HTRIXIST_BLOCK;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.E-HS-1CN.jpg" ] = HTRIXIST_HTRIXIST_E_HS_1CN;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.E-HS-1CO.jpg" ] = HTRIXIST_HTRIXIST_E_HS_1CO;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.E-HS-2.jpg" ] = HTRIXIST_HTRIXIST_E_HS_2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.E-HT-1CN.jpg" ] = HTRIXIST_HTRIXIST_E_HT_1CN;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.E-HT-1CO.jpg" ] = HTRIXIST_HTRIXIST_E_HT_1CO;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.E-HT-2.jpg" ] = HTRIXIST_HTRIXIST_E_HT_2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.F-HS-2CN.jpg" ] = HTRIXIST_HTRIXIST_F_HS_2CN;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.F-HS-2CO.jpg" ] = HTRIXIST_HTRIXIST_F_HS_2CO;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.F-HS-4.jpg" ] = HTRIXIST_HTRIXIST_F_HS_4;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.F-HT-2CN.jpg" ] = HTRIXIST_HTRIXIST_F_HT_2CN;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.F-HT-2CO.jpg" ] = HTRIXIST_HTRIXIST_F_HT_2CO;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.F-HT-4.jpg" ] = HTRIXIST_HTRIXIST_F_HT_4;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.G-HS-2.jpg" ] = HTRIXIST_HTRIXIST_G_HS_2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.G-HT-2.jpg" ] = HTRIXIST_HTRIXIST_G_HT_2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.GEN-HS.jpg" ] = HTRIXIST_HTRIXIST_GEN_HS;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.GEN-HT.jpg" ] = HTRIXIST_HTRIXIST_GEN_HT;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.H-HS-2.jpg" ] = HTRIXIST_HTRIXIST_H_HS_2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.H-HT-2.jpg" ] = HTRIXIST_HTRIXIST_H_HT_2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.J12-HS1.jpg" ] = HTRIXIST_HTRIXIST_J12_HS1;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.J12-HS2.jpg" ] = HTRIXIST_HTRIXIST_J12_HS2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.J12-HT1.jpg" ] = HTRIXIST_HTRIXIST_J12_HT1;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.J12-HT2.jpg" ] = HTRIXIST_HTRIXIST_J12_HT2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.J21-HS1.jpg" ] = HTRIXIST_HTRIXIST_J21_HS1;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.J21-HS2.jpg" ] = HTRIXIST_HTRIXIST_J21_HS2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.J21-HT1.jpg" ] = HTRIXIST_HTRIXIST_J21_HT1;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.J21-HT2.jpg" ] = HTRIXIST_HTRIXIST_J21_HT2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.K-HT-1.jpg" ] = HTRIXIST_HTRIXIST_K_HT_1;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.K-HT-2.jpg" ] = HTRIXIST_HTRIXIST_K_HT_2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.SIMP-HS.jpg" ] = HTRIXIST_HTRIXIST_SIMP_HS;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.SMP-HT.jpg" ] = HTRIXIST_HTRIXIST_SMP_HT;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.X-HS-1.jpg" ] = HTRIXIST_HTRIXIST_X_HS_1;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.X-HS-2.jpg" ] = HTRIXIST_HTRIXIST_X_HS_2;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.X-HT-1.jpg" ] = HTRIXIST_HTRIXIST_X_HT_1;
    tempIconMap["2DIcons/HTRIXIST/HTRIXIST.X-HT-2.jpg" ] = HTRIXIST_HTRIXIST_X_HT_2;
    tempIconMap["2DIcons/HXFlux/HXFlux.BLOCK.jpg" ] = HXFlux_HXFlux_BLOCK;
    tempIconMap["2DIcons/HyCyc/HyCyc.BLOCK.jpg" ] = HyCyc_HyCyc_BLOCK;
    tempIconMap["2DIcons/HyCyc/HyCyc.ICON.jpg" ] = HyCyc_HyCyc_ICON;
    tempIconMap["2DIcons/MCompr/MCompr.BLOCK.jpg" ] = MCompr_MCompr_BLOCK;
    tempIconMap["2DIcons/MCompr/MCompr.ICON1.jpg" ] = MCompr_MCompr_ICON1;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.ACONTLR.jpg" ] = Measurement_Measurement_ACONTLR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.AINDICTR.jpg" ] = Measurement_Measurement_AINDICTR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.BLOCK.jpg" ] = Measurement_Measurement_BLOCK;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.FCONTLR.jpg" ] = Measurement_Measurement_FCONTLR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.FINDICTR.jpg" ] = Measurement_Measurement_FINDICTR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.LCONTLR.jpg" ] = Measurement_Measurement_LCONTLR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.LINDICTR.jpg" ] = Measurement_Measurement_LINDICTR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.MEASURE1.jpg" ] = Measurement_Measurement_MEASURE1;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.MEASURE2.jpg" ] = Measurement_Measurement_MEASURE2;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.MEASURE3.jpg" ] = Measurement_Measurement_MEASURE3;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.MEASURE4.jpg" ] = Measurement_Measurement_MEASURE4;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.MEASURE5.jpg" ] = Measurement_Measurement_MEASURE5;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.MEASURE6.jpg" ] = Measurement_Measurement_MEASURE6;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.MEASURE7.jpg" ] = Measurement_Measurement_MEASURE7;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.MEASURE8.jpg" ] = Measurement_Measurement_MEASURE8;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.PCONTLR.jpg" ] = Measurement_Measurement_PCONTLR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.PINDICTR.jpg" ] = Measurement_Measurement_PINDICTR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.TCONTLR.jpg" ] = Measurement_Measurement_TCONTLR;
    tempIconMap["2DIcons/MEASUREMENT/MEASUREMENT.TINDICTR.jpg" ] = Measurement_Measurement_TINDICTR;
    tempIconMap["2DIcons/MHeatX/MHeatX.BLOCK.jpg" ] = MHeatX_MHeatX_BLOCK;
    tempIconMap["2DIcons/MHeatX/MHeatX.BLOCK2.jpg" ] = MHeatX_MHeatX_BLOCK2;
    tempIconMap["2DIcons/MHeatX/MHeatX.BLOCK3.jpg" ] = MHeatX_MHeatX_BLOCK3;
    tempIconMap["2DIcons/MHeatX/MHeatX.CIRC-MHX.jpg" ] = MHeatX_MHeatX_CIRC_MHX;
    tempIconMap["2DIcons/MHeatX/MHeatX.COCURNT.jpg" ] = MHeatX_MHeatX_COCURNT;
    tempIconMap["2DIcons/MHeatX/MHeatX.COUNTER.jpg" ] = MHeatX_MHeatX_COUNTER;
    tempIconMap["2DIcons/MHeatX/MHeatX.COUNTER2.jpg" ] = MHeatX_MHeatX_COUNTER2;
    tempIconMap["2DIcons/MHeatX/MHeatX.ICON1.jpg" ] = MHeatX_MHeatX_ICON1;
    tempIconMap["2DIcons/MHeatX/MHeatX.SIMP-MHX.jpg" ] = MHeatX_MHeatX_SIMP_MHX;
    tempIconMap["2DIcons/Mixer/Mixer.3WAY.jpg" ] = Mixer_Mixer_3WAY;
    tempIconMap["2DIcons/Mixer/Mixer.BLOCK.jpg" ] = Mixer_Mixer_BLOCK;
    tempIconMap["2DIcons/Mixer/Mixer.DOT.jpg" ] = Mixer_Mixer_DOT;
    tempIconMap["2DIcons/Mixer/Mixer.HEAT.jpg" ] = Mixer_Mixer_HEAT;
    tempIconMap["2DIcons/Mixer/Mixer.HOPPER.jpg" ] = Mixer_Mixer_HOPPER;
    tempIconMap["2DIcons/Mixer/Mixer.SCREW.jpg" ] = Mixer_Mixer_SCREW;
    tempIconMap["2DIcons/Mixer/Mixer.TANK.jpg" ] = Mixer_Mixer_TANK;
    tempIconMap["2DIcons/Mixer/Mixer.TEE.jpg" ] = Mixer_Mixer_TEE;
    tempIconMap["2DIcons/Mixer/Mixer.TRIANGLE.jpg" ] = Mixer_Mixer_TRIANGLE;
    tempIconMap["2DIcons/Mixer/Mixer.VALVE.jpg" ] = Mixer_Mixer_VALVE;
    tempIconMap["2DIcons/Mixer/Mixer.WORK.jpg" ] = Mixer_Mixer_WORK;
    tempIconMap["2DIcons/Mult/Mult.BLOCK.jpg" ] = Mult_Mult_BLOCK;
    tempIconMap["2DIcons/Mult/Mult.DOT.jpg" ] = Mult_Mult_DOT;
    tempIconMap["2DIcons/Mult/Mult.HEAT.jpg" ] = Mult_Mult_HEAT;
    tempIconMap["2DIcons/Mult/Mult.WORK.jpg" ] = Mult_Mult_WORK;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.AIRCOL.jpg" ] = MultiFrac_MultiFrac_AIRCOL;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.BLOCK.jpg" ] = MultiFrac_MultiFrac_BLOCK;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU1.jpg" ] = MultiFrac_MultiFrac_CDU1;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU2.jpg" ] = MultiFrac_MultiFrac_CDU2;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU3.jpg" ] = MultiFrac_MultiFrac_CDU3;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU4.jpg" ] = MultiFrac_MultiFrac_CDU4;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU5.jpg" ] = MultiFrac_MultiFrac_CDU5;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU6.jpg" ] = MultiFrac_MultiFrac_CDU6;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU7.jpg" ] = MultiFrac_MultiFrac_CDU7;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU8.jpg" ] = MultiFrac_MultiFrac_CDU8;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.CDU9.jpg" ] = MultiFrac_MultiFrac_CDU9;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.PETLYUK.jpg" ] = MultiFrac_MultiFrac_PETLYUK;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.PFRAC.jpg" ] = MultiFrac_MultiFrac_PFRAC;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.PREFLASH.jpg" ] = MultiFrac_MultiFrac_PREFLASH;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.VACUUM1.jpg" ] = MultiFrac_MultiFrac_VACUUM1;
    tempIconMap["2DIcons/MultiFrac/MultiFrac.VACUUM2.jpg" ] = MultiFrac_MultiFrac_VACUUM2;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.ABSBR.jpg" ] = PetroFrac_PetroFrac_ABSBR;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.BLOCK.jpg" ] = PetroFrac_PetroFrac_BLOCK;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU1.jpg" ] = PetroFrac_PetroFrac_CDU1;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU10.jpg" ] = PetroFrac_PetroFrac_CDU10;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU10F.jpg" ] = PetroFrac_PetroFrac_CDU10F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU11.jpg" ] = PetroFrac_PetroFrac_CDU11;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU11F.jpg" ] = PetroFrac_PetroFrac_CDU11F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU12.jpg" ] = PetroFrac_PetroFrac_CDU12;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU12F.jpg" ] = PetroFrac_PetroFrac_CDU12F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU13.jpg" ] = PetroFrac_PetroFrac_CDU13;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU13F.jpg" ] = PetroFrac_PetroFrac_CDU13F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU14.jpg" ] = PetroFrac_PetroFrac_CDU14;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU14F.jpg" ] = PetroFrac_PetroFrac_CDU14F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU15.jpg" ] = PetroFrac_PetroFrac_CDU15;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU15F.jpg" ] = PetroFrac_PetroFrac_CDU15F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU1F.jpg" ] = PetroFrac_PetroFrac_CDU1F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU2.jpg" ] = PetroFrac_PetroFrac_CDU2;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU2F.jpg" ] = PetroFrac_PetroFrac_CDU2F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU3.jpg" ] = PetroFrac_PetroFrac_CDU3;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU3F.jpg" ] = PetroFrac_PetroFrac_CDU3F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU4.jpg" ] = PetroFrac_PetroFrac_CDU4;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU4F.jpg" ] = PetroFrac_PetroFrac_CDU4F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU5.jpg" ] = PetroFrac_PetroFrac_CDU5;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU5F.jpg" ] = PetroFrac_PetroFrac_CDU5F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU6.jpg" ] = PetroFrac_PetroFrac_CDU6;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU6F.jpg" ] = PetroFrac_PetroFrac_CDU6F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU7.jpg" ] = PetroFrac_PetroFrac_CDU7;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU7F.jpg" ] = PetroFrac_PetroFrac_CDU7F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU8.jpg" ] = PetroFrac_PetroFrac_CDU8;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU8F.jpg" ] = PetroFrac_PetroFrac_CDU8F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU9.jpg" ] = PetroFrac_PetroFrac_CDU9;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.CDU9F.jpg" ] = PetroFrac_PetroFrac_CDU9F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.FCC-MF1.jpg" ] = PetroFrac_PetroFrac_FCC_MF1;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.FCC-MF2.jpg" ] = PetroFrac_PetroFrac_FCC_MF2;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.FRACT.jpg" ] = PetroFrac_PetroFrac_FRACT;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.PFRAC.jpg" ] = PetroFrac_PetroFrac_PFRAC;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.PFRACF.jpg" ] = PetroFrac_PetroFrac_PFRACF;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.PREFL1.jpg" ] = PetroFrac_PetroFrac_PREFL1;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.PREFL1F.jpg" ] = PetroFrac_PetroFrac_PREFL1F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.PREFL2.jpg" ] = PetroFrac_PetroFrac_PREFL2;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.PREFL2F.jpg" ] = PetroFrac_PetroFrac_PREFL2F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.STRIP.jpg" ] = PetroFrac_PetroFrac_STRIP;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.VACUUM1.jpg" ] = PetroFrac_PetroFrac_VACUUM1;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.VACUUM1F.jpg" ] = PetroFrac_PetroFrac_VACUUM1F;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.VACUUM2.jpg" ] = PetroFrac_PetroFrac_VACUUM2;
    tempIconMap["2DIcons/PETROFRAC/PETROFRAC.VACUUM2F.jpg" ] = PetroFrac_PetroFrac_VACUUM2F;
    tempIconMap["2DIcons/Pipe/Pipe.BLOCK.jpg" ] = Pipe_Pipe_BLOCK;
    tempIconMap["2DIcons/Pipe/Pipe.D-PIPE.jpg" ] = Pipe_Pipe_D_PIPE;
    tempIconMap["2DIcons/Pipe/Pipe.HI-PIPE.jpg" ] = Pipe_Pipe_HI_PIPE;
    tempIconMap["2DIcons/Pipe/Pipe.U-PIPE.jpg" ] = Pipe_Pipe_U_PIPE;
    tempIconMap["2DIcons/Pipe/Pipe.V-PIPE.jpg" ] = Pipe_Pipe_V_PIPE;
    tempIconMap["2DIcons/Pipeline/Pipeline.BLOCK.jpg" ] = Pipeline_Pipeline_BLOCK;
    tempIconMap["2DIcons/Pipeline/Pipeline.D-PIPE.jpg" ] = Pipeline_Pipeline_D_PIPE;
    tempIconMap["2DIcons/Pipeline/Pipeline.HI-PIPE.jpg" ] = Pipeline_Pipeline_HI_PIPE;
    tempIconMap["2DIcons/Pipeline/Pipeline.U-PIPE.jpg" ] = Pipeline_Pipeline_U_PIPE;
    tempIconMap["2DIcons/Pipeline/Pipeline.V-PIPE.jpg" ] = Pipeline_Pipeline_V_PIPE;
    tempIconMap["2DIcons/Pump/Pump.BLOCK.jpg" ] = Pump_Pump_BLOCK;
    tempIconMap["2DIcons/Pump/Pump.ICON1.jpg" ] = Pump_Pump_ICON1;
    tempIconMap["2DIcons/Pump/Pump.ICON2.jpg" ] = Pump_Pump_ICON2;
    tempIconMap["2DIcons/Qtvec/Qtvec.BLOCK.jpg" ] = Qtvec_Qtvec_BLOCK;
    tempIconMap["2DIcons/Qtvec/Qtvec.DOT.jpg" ] = Qtvec_Qtvec_DOT;
    tempIconMap["2DIcons/RadFrac/RadFrac.ABSBR1.jpg" ] = RadFrac_RadFrac_ABSBR1;
    tempIconMap["2DIcons/RadFrac/RadFrac.ABSBR2.jpg" ] = RadFrac_RadFrac_ABSBR2;
    tempIconMap["2DIcons/RadFrac/RadFrac.ABSBR3.jpg" ] = RadFrac_RadFrac_ABSBR3;
    tempIconMap["2DIcons/RadFrac/RadFrac.BLOCK.jpg" ] = RadFrac_RadFrac_BLOCK;
    tempIconMap["2DIcons/RadFrac/RadFrac.DECANT1.jpg" ] = RadFrac_RadFrac_DECANT1;
    tempIconMap["2DIcons/RadFrac/RadFrac.DECANT2.jpg" ] = RadFrac_RadFrac_DECANT2;
    tempIconMap["2DIcons/RadFrac/RadFrac.DECANT3.jpg" ] = RadFrac_RadFrac_DECANT3;
    tempIconMap["2DIcons/RadFrac/RadFrac.FRACT1.jpg" ] = RadFrac_RadFrac_FRACT1;
    tempIconMap["2DIcons/RadFrac/RadFrac.FRACT2.jpg" ] = RadFrac_RadFrac_FRACT2;
    tempIconMap["2DIcons/RadFrac/RadFrac.PACKABS.jpg" ] = RadFrac_RadFrac_PACKABS;
    tempIconMap["2DIcons/RadFrac/RadFrac.PACKCOL1.jpg" ] = RadFrac_RadFrac_PACKCOL1;
    tempIconMap["2DIcons/RadFrac/RadFrac.PACKCOL2.jpg" ] = RadFrac_RadFrac_PACKCOL2;
    tempIconMap["2DIcons/RadFrac/RadFrac.PACKSTR1.jpg" ] = RadFrac_RadFrac_PACKSTR1;
    tempIconMap["2DIcons/RadFrac/RadFrac.PACKSTR2.jpg" ] = RadFrac_RadFrac_PACKSTR2;
    tempIconMap["2DIcons/RadFrac/RadFrac.RECT.jpg" ] = RadFrac_RadFrac_RECT;
    tempIconMap["2DIcons/RadFrac/RadFrac.STRIP1.jpg" ] = RadFrac_RadFrac_STRIP1;
    tempIconMap["2DIcons/RadFrac/RadFrac.STRIP2.jpg" ] = RadFrac_RadFrac_STRIP2;
    tempIconMap["2DIcons/RateFrac/RateFrac.ABSBR2.jpg" ] = RateFrac_RateFrac_ABSBR2;
    tempIconMap["2DIcons/RateFrac/RateFrac.ABSBR3.jpg" ] = RateFrac_RateFrac_ABSBR3;
    tempIconMap["2DIcons/RateFrac/RateFrac.ABSORBER.jpg" ] = RateFrac_RateFrac_ABSORBER;
    tempIconMap["2DIcons/RateFrac/RateFrac.BLOCK.jpg" ] = RateFrac_RateFrac_BLOCK;
    tempIconMap["2DIcons/RateFrac/RateFrac.FRACT.jpg" ] = RateFrac_RateFrac_FRACT;
    tempIconMap["2DIcons/RateFrac/RateFrac.PACKABS.jpg" ] = RateFrac_RateFrac_PACKABS;
    tempIconMap["2DIcons/RateFrac/RateFrac.PACKCOL.jpg" ] = RateFrac_RateFrac_PACKCOL;
    tempIconMap["2DIcons/RateFrac/RateFrac.PACKSTR.jpg" ] = RateFrac_RateFrac_PACKSTR;
    tempIconMap["2DIcons/RateFrac/RateFrac.RECT.jpg" ] = RateFrac_RateFrac_RECT;
    tempIconMap["2DIcons/RateFrac/RateFrac.STRIPPER.jpg" ] = RateFrac_RateFrac_STRIPPER;
    tempIconMap["2DIcons/RateFrac/RateFrac.VACUUM1.jpg" ] = RateFrac_RateFrac_VACUUM1;
    tempIconMap["2DIcons/RateFrac/RateFrac.VACUUM2.jpg" ] = RateFrac_RateFrac_VACUUM2;
    tempIconMap["2DIcons/RBatch/RBatch.BLOCK.jpg" ] = RBatch_RBatch_BLOCK;
    tempIconMap["2DIcons/RBatch/RBatch.ICON1.jpg" ] = RBatch_RBatch_ICON1;
    tempIconMap["2DIcons/RCSTR/RCSTR.BLOCK.jpg" ] = RCSTR_RCSTR_BLOCK;
    tempIconMap["2DIcons/RCSTR/RCSTR.ICON1.jpg" ] = RCSTR_RCSTR_ICON1;
    tempIconMap["2DIcons/REquil/REquil.BLOCK.jpg" ] = REquil_REquil_BLOCK;
    tempIconMap["2DIcons/REquil/REquil.ICON2.jpg" ] = REquil_REquil_ICON2;
    tempIconMap["2DIcons/REquil/REquil.ICON3.jpg" ] = REquil_REquil_ICON3;
    tempIconMap["2DIcons/RGibbs/RGibbs.BLOCK.jpg" ] = RGibbs_RGibbs_BLOCK;
    tempIconMap["2DIcons/RGibbs/RGibbs.ICON1.jpg" ] = RGibbs_RGibbs_ICON1;
    tempIconMap["2DIcons/RGibbs/RGibbs.ICON2.jpg" ] = RGibbs_RGibbs_ICON2;
    tempIconMap["2DIcons/RPlug/RPlug.BLOCK.jpg" ] = RPlug_RPlug_BLOCK;
    tempIconMap["2DIcons/RPlug/RPlug.ICON1.jpg" ] = RPlug_RPlug_ICON1;
    tempIconMap["2DIcons/RPlug/RPlug.ICON2.jpg" ] = RPlug_RPlug_ICON2;
    tempIconMap["2DIcons/RPlug/RPlug.ICON3.jpg" ] = RPlug_RPlug_ICON3;
    tempIconMap["2DIcons/RStoic/RStoic.BLOCK.jpg" ] = RStoic_RStoic_BLOCK;
    tempIconMap["2DIcons/RStoic/RStoic.ICON1.jpg" ] = RStoic_RStoic_ICON1;
    tempIconMap["2DIcons/RStoic/RStoic.ICON2.jpg" ] = RStoic_RStoic_ICON2;
    tempIconMap["2DIcons/RStoic/RStoic.ICON3.jpg" ] = RStoic_RStoic_ICON3;
    tempIconMap["2DIcons/RStoic/RStoic.ICON4.jpg" ] = RStoic_RStoic_ICON4;
    tempIconMap["2DIcons/RYield/RYield.BLOCK.jpg" ] = RYield_RYield_BLOCK;
    tempIconMap["2DIcons/RYield/RYield.ICON2.jpg" ] = RYield_RYield_ICON2;
    tempIconMap["2DIcons/RYield/RYield.ICON3.jpg" ] = RYield_RYield_ICON3;
    tempIconMap["2DIcons/SCFrac/SCFrac.BLOCK.jpg" ] = SCFrac_SCFrac_BLOCK;
    tempIconMap["2DIcons/SCFrac/SCFrac.CDU1.jpg" ] = SCFrac_SCFrac_CDU1;
    tempIconMap["2DIcons/SCFrac/SCFrac.CDU2.jpg" ] = SCFrac_SCFrac_CDU2;
    tempIconMap["2DIcons/SCFrac/SCFrac.CDU3.jpg" ] = SCFrac_SCFrac_CDU3;
    tempIconMap["2DIcons/SCFrac/SCFrac.VACUUM1.jpg" ] = SCFrac_SCFrac_VACUUM1;
    tempIconMap["2DIcons/SCFrac/SCFrac.VACUUM2.jpg" ] = SCFrac_SCFrac_VACUUM2;
    tempIconMap["2DIcons/Screen/Screen.BLOCK.jpg" ] = Screen_Screen_BLOCK;
    tempIconMap["2DIcons/Screen/Screen.ICON1.jpg" ] = Screen_Screen_ICON1;
    tempIconMap["2DIcons/Screen/Screen.ICON2.jpg" ] = Screen_Screen_ICON2;
    tempIconMap["2DIcons/Selector/Selector.BLOCK.jpg" ] = Selector_Selector_BLOCK;
    tempIconMap["2DIcons/Selector/Selector.HEAT.jpg" ] = Selector_Selector_HEAT;
    tempIconMap["2DIcons/Selector/Selector.TRIANGLE.jpg" ] = Selector_Selector_TRIANGLE;
    tempIconMap["2DIcons/Selector/Selector.WORK.jpg" ] = Selector_Selector_WORK;
    tempIconMap["2DIcons/Sep/Sep.BLOCK.jpg" ] = Sep2_Sep2_BLOCK;
    tempIconMap["2DIcons/Sep/Sep.ICON1.jpg" ] = Sep2_Sep2_ICON1;
    tempIconMap["2DIcons/Sep/Sep.ICON2.jpg" ] = Sep2_Sep2_ICON2;
    tempIconMap["2DIcons/Sep/Sep.ICON3.jpg" ] = Sep2_Sep2_ICON3;
    tempIconMap["2DIcons/Sep2/Sep2.BLOCK.jpg" ] = Sep_Sep_BLOCK;
    tempIconMap["2DIcons/Sep2/Sep2.ICON1.jpg" ] = Sep_Sep_ICON1;
    tempIconMap["2DIcons/Sep2/Sep2.ICON2.jpg" ] = Sep_Sep_ICON2;
    tempIconMap["2DIcons/Sep2/Sep2.ICON3.jpg" ] = Sep_Sep_ICON3;
    tempIconMap["2DIcons/SSplit/SSplit.3WAY.jpg" ] = SSplit_SSplit_3WAY;
    tempIconMap["2DIcons/SSplit/SSplit.BLOCK.jpg" ] = SSplit_SSplit_BLOCK;
    tempIconMap["2DIcons/SSplit/SSplit.CCD.jpg" ] = SSplit_SSplit_CCD;
    tempIconMap["2DIcons/SSplit/SSplit.CFUGE.jpg" ] = SSplit_SSplit_CFUGE;
    tempIconMap["2DIcons/SSplit/SSplit.CYCLONE.jpg" ] = SSplit_SSplit_CYCLONE;
    tempIconMap["2DIcons/SSplit/SSplit.DOT.jpg" ] = SSplit_SSplit_DOT;
    tempIconMap["2DIcons/SSplit/SSplit.FILTER1.jpg" ] = SSplit_SSplit_FILTER1;
    tempIconMap["2DIcons/SSplit/SSplit.FILTER2.jpg" ] = SSplit_SSplit_FILTER2;
    tempIconMap["2DIcons/SSplit/SSplit.SCREEN.jpg" ] = SSplit_SSplit_SCREEN;
    tempIconMap["2DIcons/SSplit/SSplit.TEE.jpg" ] = SSplit_SSplit_TEE;
    tempIconMap["2DIcons/SSplit/SSplit.TRIANGLE.jpg" ] = SSplit_SSplit_TRIANGLE;
    tempIconMap["2DIcons/SSplit/SSplit.VSCRUB.jpg" ] = SSplit_SSplit_VSCRUB;
    tempIconMap["2DIcons/SWash/SWash.BLOCK.jpg" ] = SWash_SWash_BLOCK;
    tempIconMap["2DIcons/SWash/SWash.ICON.jpg" ] = SWash_SWash_ICON;
    tempIconMap["2DIcons/User/User.BLOCK.jpg" ] = User2_User2_BLOCK;
    tempIconMap["2DIcons/User/User.SMALL.jpg" ] = User2_User2_CFUGE;
    tempIconMap["2DIcons/USER2/USER2.BLOCK.jpg" ] = User2_User2_CSTR;
    tempIconMap["2DIcons/USER2/USER2.CFUGE.jpg" ] = User2_User2_EXCEL;
    tempIconMap["2DIcons/USER2/USER2.CSTR.jpg" ] = User2_User2_FILTER;
    tempIconMap["2DIcons/USER2/USER2.EXCEL.jpg" ] = User2_User2_FRACT;
    tempIconMap["2DIcons/USER2/USER2.FILTER.jpg" ] = User2_User2_H_DRUM;
    tempIconMap["2DIcons/USER2/USER2.FRACT.jpg" ] = User2_User2_HEATER;
    tempIconMap["2DIcons/USER2/USER2.H-DRUM.jpg" ] = User2_User2_HEATX1;
    tempIconMap["2DIcons/USER2/USER2.HEATER.jpg" ] = User2_User2_HEATX2;
    tempIconMap["2DIcons/USER2/USER2.HEATX1.jpg" ] = User2_User2_PLUG;
    tempIconMap["2DIcons/USER2/USER2.HEATX2.jpg" ] = User2_User2_REACTOR;
    tempIconMap["2DIcons/USER2/USER2.PLUG.jpg" ] = User2_User2_RECT;
    tempIconMap["2DIcons/USER2/USER2.REACTOR.jpg" ] = User2_User2_STRIP;
    tempIconMap["2DIcons/USER2/USER2.RECT.jpg" ] = User2_User2_V_DRUM;
    tempIconMap["2DIcons/USER2/USER2.STRIP.jpg" ] = User2_User2_VALVE4;
    tempIconMap["2DIcons/USER2/USER2.V-DRUM.jpg" ] = User3_User3_BLOCK;
    tempIconMap["2DIcons/USER2/USER2.VALVE4.jpg" ] = User3_User3_CFUGE;
    tempIconMap["2DIcons/USER3/USER3.BLOCK.jpg" ] = User3_User3_CSTR;
    tempIconMap["2DIcons/USER3/USER3.CFUGE.jpg" ] = User3_User3_FILTER;
    tempIconMap["2DIcons/USER3/USER3.CSTR.jpg" ] = User3_User3_FRACT;
    tempIconMap["2DIcons/USER3/USER3.FILTER.jpg" ] = User3_User3_H_DRUM;
    tempIconMap["2DIcons/USER3/USER3.FRACT.jpg" ] = User3_User3_HEATER;
    tempIconMap["2DIcons/USER3/USER3.H-DRUM.jpg" ] = User3_User3_HEATX1;
    tempIconMap["2DIcons/USER3/USER3.HEATER.jpg" ] = User3_User3_HEATX2;
    tempIconMap["2DIcons/USER3/USER3.HEATX1.jpg" ] = User3_User3_PLUG;
    tempIconMap["2DIcons/USER3/USER3.HEATX2.jpg" ] = User3_User3_REACTOR;
    tempIconMap["2DIcons/USER3/USER3.PLUG.jpg" ] = User3_User3_RECT;
    tempIconMap["2DIcons/USER3/USER3.REACTOR.jpg" ] = User3_User3_STRIP;
    tempIconMap["2DIcons/USER3/USER3.RECT.jpg" ] = User3_User3_V_DRUM;
    tempIconMap["2DIcons/USER3/USER3.STRIP.jpg" ] = User3_User3_VALVE4;
    tempIconMap["2DIcons/USER3/USER3.V-DRUM.jpg" ] = User_User_BLOCK;
    tempIconMap["2DIcons/USER3/USER3.VALVE4.jpg" ] = User_User_SMALL;
    tempIconMap["2DIcons/VALVE/VALVE.BLOCK.jpg" ] = Valve_Valve_BLOCK;
    tempIconMap["2DIcons/VALVE/VALVE.VALVE1.jpg" ] = Valve_Valve_VALVE1;
    tempIconMap["2DIcons/VALVE/VALVE.VALVE2.jpg" ] = Valve_Valve_VALVE2;
    tempIconMap["2DIcons/VALVE/VALVE.VALVE3.jpg" ] = Valve_Valve_VALVE3;
    tempIconMap["2DIcons/VALVE/VALVE.VALVE4.jpg" ] = Valve_Valve_VALVE4;
    tempIconMap["2DIcons/VSCRUB/VSCRUB.BLOCK.jpg" ] = VScrub_VScrub_BLOCK;
    tempIconMap["2DIcons/VSCRUB/VSCRUB.ICON.jpg" ] = VScrub_VScrub_ICON;
    return tempIconMap;
}
