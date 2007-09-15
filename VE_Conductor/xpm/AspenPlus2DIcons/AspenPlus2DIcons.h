#include <map>
#include <string>

#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_ECN_HS_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_ECN_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_ECN_HT_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_ECN_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_FORCED_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_FORCED_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_INDUCE_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_INDUCE_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_NATURA_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Aerotran_Aerotran_NATURA_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Analyzer_Analyzer_ANALYZE2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Analyzer_Analyzer_ANALYZER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Analyzer_Analyzer_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/BatchFrac_BatchFrac_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/BatchFrac_BatchFrac_COLUMN1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/BatchFrac_BatchFrac_COLUMN2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/BatchFrac_BatchFrac_COLUMN3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/CCD_CCD_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/CCD_CCD_ICON.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/CFuge_CFuge_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/CFuge_CFuge_ICON.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/ClChng_ClChng_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/ClChng_ClChng_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Compr_Compr_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Compr_Compr_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Compr_Compr_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Compr_Compr_ICON3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crusher_Crusher_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crusher_Crusher_ICON.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crystallizer_Crystallizer_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crystallizer_Crystallizer_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Crystallizer_Crystallizer_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Cyclone_Cyclone_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Cyclone_Cyclone_ICON.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Decanter_Decanter_V_DRUM.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Decanter_Decanter_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Decanter_Decanter_H_DRUM.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Distl_Distl_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Distl_Distl_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Distl_Distl_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dryer_Dryer_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dryer_Dryer_FLUIDBED.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dryer_Dryer_FLUIDBED2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dryer_Dryer_SPRAY.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/DSTWU_DSTWU_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/DSTWU_DSTWU_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/DSTWU_DSTWU_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dupl_Dupl_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dupl_Dupl_DOT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dupl_Dupl_HEAT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Dupl_Dupl_WORK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/ESP_ESP_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/ESP_ESP_ICON.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Extract_Extract_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Extract_Extract_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Extract_Extract_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Extract_Extract_POD.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FabFl_FabFl_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FabFl_FabFl_ICON.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Feedbl_Feedbl_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Feedbl_Feedbl_FEEDBL.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Filter_Filter_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Filter_Filter_PLATE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Filter_Filter_ROTARY.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_FURNACE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_H_DRUM.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_HEATER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_V_DRUM1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash2_Flash2_V_DRUM2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_FURNACE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_H_DRUM.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_HEATER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_V_DRUM1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Flash3_Flash3_V_DRUM2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_3WAY.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_DOT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_HEAT_TEE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_HEAT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_TEE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_TRIANGLE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_WORK_TEE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/FSplit_FSplit_WORK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_AIRCOOLER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_COMPR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_FURNACE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_Heater.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_PUMP.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_VALVE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_VALVE2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Heater_Heater_VALVE4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HS_1CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HS_1CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HT_1CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HT_1CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_E_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_ECN_HS_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_ECN_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_ECN_HT_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_ECN_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HS_2CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HS_2CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HS_4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HT_2CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HT_2CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_F_HT_4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_FORCED_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_FORCED_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_G_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_G_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_GEN_HS.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_GEN_HT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_H_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_H_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_INDUCE_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_INDUCE_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J12_HS1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J12_HS2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J12_HT1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J12_HT2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J21_HS1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J21_HS2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J21_HT1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_J21_HT2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_K_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_NATURA_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_NATURA_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_SIMP_HS.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_SIMP_HT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_X_HS_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_X_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_X_HT_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HeatX_HeatX_X_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HS_1CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HS_1CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HT_1CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HT_1CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_E_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HS_2CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HS_2CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HS_4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HT_2CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HT_2CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_F_HT_4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_G_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_G_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_GEN_HS.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_GEN_HT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_H_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_H_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J12_HS1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J12_HS2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J12_HT1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J12_HT2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J21_HS1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J21_HS2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J21_HT1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_J21_HT2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_K_HT_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_K_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_SIMP_HS.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_SMP_HT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_X_HS_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_X_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_X_HT_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hetran_Hetran_X_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Hierarchy_Hierarchy_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HS_1CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HS_1CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HT_1CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HT_1CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_E_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HS_2CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HS_2CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HS_4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HT_2CN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HT_2CO.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_F_HT_4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_G_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_G_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_GEN_HS.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_GEN_HT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_H_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_H_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J12_HS1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J12_HS2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J12_HT1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J12_HT2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J21_HS1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J21_HS2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J21_HT1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_J21_HT2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_K_HT_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_K_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_SIMP_HS.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_SMP_HT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_X_HS_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_X_HS_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_X_HT_1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HTRIXIST_HTRIXIST_X_HT_2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HXFlux_HXFlux_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HyCyc_HyCyc_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/HyCyc_HyCyc_ICON.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MCompr_MCompr_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MCompr_MCompr_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_ACONTLR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_AINDICTR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_FCONTLR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_FINDICTR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_LCONTLR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_LINDICTR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE5.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE6.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE7.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_MEASURE8.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_PCONTLR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_PINDICTR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_TCONTLR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Measurement_Measurement_TINDICTR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_BLOCK2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_BLOCK3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_CIRC_MHX.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_COCURNT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_COUNTER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_COUNTER2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MHeatX_MHeatX_SIMP_MHX.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_3WAY.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_DOT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_HEAT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_HOPPER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_SCREW.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_TANK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_TEE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_TRIANGLE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_VALVE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mixer_Mixer_WORK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mult_Mult_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mult_Mult_DOT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mult_Mult_HEAT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Mult_Mult_WORK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_AIRCOL.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU5.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU6.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU7.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU8.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_CDU9.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_PETLYUK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_PFRAC.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_PREFLASH.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_VACUUM1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/MultiFrac_MultiFrac_VACUUM2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_ABSBR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU10.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU10F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU11.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU11F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU12.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU12F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU13.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU13F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU14.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU14F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU15.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU15F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU1F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU2F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU3F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU4F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU5.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU5F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU6.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU6F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU7.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU7F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU8.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU8F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU9.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_CDU9F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_FCC_MF1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_FCC_MF2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_FRACT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PFRAC.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PFRACF.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PREFL1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PREFL1F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PREFL2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_PREFL2F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_STRIP.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_VACUUM1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_VACUUM1F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_VACUUM2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/PetroFrac_PetroFrac_VACUUM2F.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_D_PIPE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_HI_PIPE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_U_PIPE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipe_Pipe_V_PIPE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_D_PIPE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_HI_PIPE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_U_PIPE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pipeline_Pipeline_V_PIPE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pump_Pump_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pump_Pump_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Pump_Pump_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Qtvec_Qtvec_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Qtvec_Qtvec_DOT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_ABSBR1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_ABSBR2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_ABSBR3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_DECANT1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_DECANT2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_DECANT3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_FRACT1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_FRACT2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKABS.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKCOL1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKCOL2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKSTR1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_PACKSTR2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_RECT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_STRIP1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RadFrac_RadFrac_STRIP2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_ABSBR2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_ABSBR3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_ABSORBER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_FRACT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_PACKABS.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_PACKCOL.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_PACKSTR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_RECT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_STRIPPER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_VACUUM1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RateFrac_RateFrac_VACUUM2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RBatch_RBatch_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RBatch_RBatch_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RCSTR_RCSTR_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RCSTR_RCSTR_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/REquil_REquil_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/REquil_REquil_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/REquil_REquil_ICON3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RGibbs_RGibbs_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RGibbs_RGibbs_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RGibbs_RGibbs_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RPlug_RPlug_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RPlug_RPlug_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RPlug_RPlug_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RPlug_RPlug_ICON3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_ICON3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RStoic_RStoic_ICON4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RYield_RYield_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RYield_RYield_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/RYield_RYield_ICON3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_CDU1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_CDU2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_CDU3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_VACUUM1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SCFrac_SCFrac_VACUUM2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Screen_Screen_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Screen_Screen_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Screen_Screen_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Selector_Selector_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Selector_Selector_HEAT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Selector_Selector_TRIANGLE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Selector_Selector_WORK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep2_Sep2_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep2_Sep2_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep2_Sep2_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep2_Sep2_ICON3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep_Sep_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep_Sep_ICON1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep_Sep_ICON2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Sep_Sep_ICON3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_3WAY.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_CCD.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_CFUGE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_CYCLONE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_DOT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_FILTER1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_FILTER2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_SCREEN.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_TEE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_TRIANGLE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SSplit_SSplit_VSCRUB.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SWash_SWash_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/SWash_SWash_ICON.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_CFUGE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_CSTR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_EXCEL.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_FILTER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_FRACT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_H_DRUM.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_HEATER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_HEATX1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_HEATX2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_PLUG.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_REACTOR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_RECT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_STRIP.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_V_DRUM.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User2_User2_VALVE4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_CFUGE.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_CSTR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_FILTER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_FRACT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_H_DRUM.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_HEATER.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_HEATX1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_HEATX2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_PLUG.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_REACTOR.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_RECT.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_STRIP.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_V_DRUM.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User3_User3_VALVE4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User_User_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/User_User_SMALL.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_VALVE1.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_VALVE2.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_VALVE3.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/Valve_Valve_VALVE4.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/VScrub_VScrub_BLOCK.h"
#include "VE_Conductor/xpm/AspenPlus2DIcons/VScrub_VScrub_ICON.h"

std::map< std::string, std::string > GetAspenPlusIconMap()
{
    std::map< std::string, std::string > tempIconMap;
/*./2DIcons/Aerotran/Aerotran.BLOCK.jpg
./2DIcons/Aerotran/Aerotran.ECN-HS-1.jpg
./2DIcons/Aerotran/Aerotran.ECN-HS-2.jpg
./2DIcons/Aerotran/Aerotran.ECN-HT-1.jpg
./2DIcons/Aerotran/Aerotran.ECN-HT-2.jpg
./2DIcons/Aerotran/Aerotran.FORCED-1.jpg
./2DIcons/Aerotran/Aerotran.FORCED-2.jpg
./2DIcons/Aerotran/Aerotran.INDUCE-1.jpg
./2DIcons/Aerotran/Aerotran.INDUCE-2.jpg
./2DIcons/Aerotran/Aerotran.NATURA-1.jpg
./2DIcons/Aerotran/Aerotran.NATURA-2.jpg
./2DIcons/Analyzer/Analyzer.ANALYZE2.jpg
./2DIcons/Analyzer/Analyzer.ANALYZER.jpg
./2DIcons/Analyzer/Analyzer.BLOCK.jpg
./2DIcons/BatchFrac/BatchFrac.BLOCK.jpg
./2DIcons/BatchFrac/BatchFrac.COLUMN1.jpg
./2DIcons/BatchFrac/BatchFrac.COLUMN2.jpg
./2DIcons/BatchFrac/BatchFrac.COLUMN3.jpg
./2DIcons/CCD/CCD.BLOCK.jpg
./2DIcons/CCD/CCD.ICON.jpg
./2DIcons/CFuge/CFuge.BLOCK.jpg
./2DIcons/CFuge/CFuge.ICON.jpg
./2DIcons/ClChng/ClChng.BLOCK.jpg
./2DIcons/ClChng/ClChng.ICON1.jpg
./2DIcons/Compr/Compr.BLOCK.jpg
./2DIcons/Compr/Compr.ICON1.jpg
./2DIcons/Compr/Compr.ICON2.jpg
./2DIcons/Compr/Compr.ICON3.jpg
./2DIcons/Crusher/Crusher.BLOCK.jpg
./2DIcons/Crusher/Crusher.ICON.jpg
./2DIcons/Crystallizer/Crystallizer.BLOCK.jpg
./2DIcons/Crystallizer/Crystallizer.ICON1.jpg
./2DIcons/Crystallizer/Crystallizer.ICON2.jpg
./2DIcons/Cyclone/Cyclone.BLOCK.jpg
./2DIcons/Cyclone/Cyclone.ICON.jpg
./2DIcons/Decanter/Decanter-V-DRUM.jpg
./2DIcons/Decanter/Decanter.BLOCK.jpg
./2DIcons/Decanter/Decanter.H-DRUM.jpg
./2DIcons/Distl/Distl.BLOCK.jpg
./2DIcons/Distl/Distl.ICON1.jpg
./2DIcons/Distl/Distl.ICON2.jpg
./2DIcons/Dryer/Dryer.BLOCK.jpg
./2DIcons/Dryer/Dryer.FLUIDBED.jpg
./2DIcons/Dryer/Dryer.FLUIDBED2.jpg
./2DIcons/Dryer/Dryer.SPRAY.jpg
./2DIcons/DSTWU/DSTWU.BLOCK.jpg
./2DIcons/DSTWU/DSTWU.ICON1.jpg
./2DIcons/DSTWU/DSTWU.ICON2.jpg
./2DIcons/Dupl/Dupl.BLOCK.jpg
./2DIcons/Dupl/Dupl.DOT.jpg
./2DIcons/Dupl/Dupl.HEAT.jpg
./2DIcons/Dupl/Dupl.WORK.jpg
./2DIcons/ESP/ESP.BLOCK.jpg
./2DIcons/ESP/ESP.ICON.jpg
./2DIcons/Extract/Extract.BLOCK.jpg
./2DIcons/Extract/Extract.ICON1.jpg
./2DIcons/Extract/Extract.ICON2.jpg
./2DIcons/Extract/Extract.POD.jpg
./2DIcons/FabFl/FabFl.BLOCK.jpg
./2DIcons/FabFl/FabFl.ICON.jpg
./2DIcons/Feedbl/Feedbl.BLOCK.jpg
./2DIcons/Feedbl/Feedbl.FEEDBL.jpg
./2DIcons/Filter/Filter.BLOCK.jpg
./2DIcons/Filter/Filter.PLATE.jpg
./2DIcons/Filter/Filter.ROTARY.jpg
./2DIcons/Flash2/Flash2.BLOCK.jpg
./2DIcons/Flash2/Flash2.FURNACE.jpg
./2DIcons/Flash2/Flash2.H-DRUM.jpg
./2DIcons/Flash2/Flash2.HEATER.jpg
./2DIcons/Flash2/Flash2.V-DRUM1.jpg
./2DIcons/Flash2/Flash2.V-DRUM2.jpg
./2DIcons/Flash3/Flash3.BLOCK.jpg
./2DIcons/Flash3/Flash3.FURNACE.jpg
./2DIcons/Flash3/Flash3.H-DRUM.jpg
./2DIcons/Flash3/Flash3.HEATER.jpg
./2DIcons/Flash3/Flash3.V-DRUM1.jpg
./2DIcons/Flash3/Flash3.V-DRUM2.jpg
./2DIcons/FSplit/FSplit.3WAY.jpg
./2DIcons/FSplit/FSplit.BLOCK.jpg
./2DIcons/FSplit/FSplit.DOT.jpg
./2DIcons/FSplit/FSplit.HEAT-TEE.jpg
./2DIcons/FSplit/FSplit.HEAT.jpg
./2DIcons/FSplit/FSplit.TEE.jpg
./2DIcons/FSplit/FSplit.TRIANGLE.jpg
./2DIcons/FSplit/FSplit.WORK-TEE.jpg
./2DIcons/FSplit/FSplit.WORK.jpg
./2DIcons/Heater/Heater.AIRCOOLER.jpg
./2DIcons/Heater/Heater.BLOCK.jpg
./2DIcons/Heater/Heater.COMPR.jpg
./2DIcons/Heater/Heater.FURNACE.jpg
./2DIcons/Heater/Heater.Heater.jpg
./2DIcons/Heater/Heater.PUMP.jpg
./2DIcons/Heater/Heater.VALVE.jpg
./2DIcons/Heater/Heater.VALVE2.jpg
./2DIcons/Heater/Heater.VALVE4.jpg
./2DIcons/HeatX/HeatX.BLOCK.jpg
./2DIcons/HeatX/HeatX.E-HS-1CN.jpg
./2DIcons/HeatX/HeatX.E-HS-1CO.jpg
./2DIcons/HeatX/HeatX.E-HS-2.jpg
./2DIcons/HeatX/HeatX.E-HT-1CN.jpg
./2DIcons/HeatX/HeatX.E-HT-1CO.jpg
./2DIcons/HeatX/HeatX.E-HT-2.jpg
./2DIcons/HeatX/HeatX.ECN-HS-1.jpg
./2DIcons/HeatX/HeatX.ECN-HS-2.jpg
./2DIcons/HeatX/HeatX.ECN-HT-1.jpg
./2DIcons/HeatX/HeatX.ECN-HT-2.jpg
./2DIcons/HeatX/HeatX.F-HS-2CN.jpg
./2DIcons/HeatX/HeatX.F-HS-2CO.jpg
./2DIcons/HeatX/HeatX.F-HS-4.jpg
./2DIcons/HeatX/HeatX.F-HT-2CN.jpg
./2DIcons/HeatX/HeatX.F-HT-2CO.jpg
./2DIcons/HeatX/HeatX.F-HT-4.jpg
./2DIcons/HeatX/HeatX.FORCED-1.jpg
./2DIcons/HeatX/HeatX.FORCED-2.jpg
./2DIcons/HeatX/HeatX.G-HS-2.jpg
./2DIcons/HeatX/HeatX.G-HT-2.jpg
./2DIcons/HeatX/HeatX.GEN-HS.jpg
./2DIcons/HeatX/HeatX.GEN-HT.jpg
./2DIcons/HeatX/HeatX.H-HS-2.jpg
./2DIcons/HeatX/HeatX.H-HT-2.jpg
./2DIcons/HeatX/HeatX.INDUCE-1.jpg
./2DIcons/HeatX/HeatX.INDUCE-2.jpg
./2DIcons/HeatX/HeatX.J12-HS1.jpg
./2DIcons/HeatX/HeatX.J12-HS2.jpg
./2DIcons/HeatX/HeatX.J12-HT1.jpg
./2DIcons/HeatX/HeatX.J12-HT2.jpg
./2DIcons/HeatX/HeatX.J21-HS1.jpg
./2DIcons/HeatX/HeatX.J21-HS2.jpg
./2DIcons/HeatX/HeatX.J21-HT1.jpg
./2DIcons/HeatX/HeatX.J21-HT2.jpg
./2DIcons/HeatX/HeatX.K-HT-2.jpg
./2DIcons/HeatX/HeatX.NATURA-1.jpg
./2DIcons/HeatX/HeatX.NATURA-2.jpg
./2DIcons/HeatX/HeatX.SIMP-HS.jpg
./2DIcons/HeatX/HeatX.SIMP-HT.jpg
./2DIcons/HeatX/HeatX.X-HS-1.jpg
./2DIcons/HeatX/HeatX.X-HS-2.jpg
./2DIcons/HeatX/HeatX.X-HT-1.jpg
./2DIcons/HeatX/HeatX.X-HT-2.jpg
./2DIcons/Hetran/Hetran.BLOCK.jpg
./2DIcons/Hetran/Hetran.E-HS-1CN.jpg
./2DIcons/Hetran/Hetran.E-HS-1CO.jpg
./2DIcons/Hetran/Hetran.E-HS-2.jpg
./2DIcons/Hetran/Hetran.E-HT-1CN.jpg
./2DIcons/Hetran/Hetran.E-HT-1CO.jpg
./2DIcons/Hetran/Hetran.E-HT-2.jpg
./2DIcons/Hetran/Hetran.F-HS-2CN.jpg
./2DIcons/Hetran/Hetran.F-HS-2CO.jpg
./2DIcons/Hetran/Hetran.F-HS-4.jpg
./2DIcons/Hetran/Hetran.F-HT-2CN.jpg
./2DIcons/Hetran/Hetran.F-HT-2CO.jpg
./2DIcons/Hetran/Hetran.F-HT-4.jpg
./2DIcons/Hetran/Hetran.G-HS-2.jpg
./2DIcons/Hetran/Hetran.G-HT-2.jpg
./2DIcons/Hetran/Hetran.GEN-HS.jpg
./2DIcons/Hetran/Hetran.GEN-HT.jpg
./2DIcons/Hetran/Hetran.H-HS-2.jpg
./2DIcons/Hetran/Hetran.H-HT-2.jpg
./2DIcons/Hetran/Hetran.J12-HS1.jpg
./2DIcons/Hetran/Hetran.J12-HS2.jpg
./2DIcons/Hetran/Hetran.J12-HT1.jpg
./2DIcons/Hetran/Hetran.J12-HT2.jpg
./2DIcons/Hetran/Hetran.J21-HS1.jpg
./2DIcons/Hetran/Hetran.J21-HS2.jpg
./2DIcons/Hetran/Hetran.J21-HT1.jpg
./2DIcons/Hetran/Hetran.J21-HT2.jpg
./2DIcons/Hetran/Hetran.K-HT-1.jpg
./2DIcons/Hetran/Hetran.K-HT-2.jpg
./2DIcons/Hetran/Hetran.SIMP-HS.jpg
./2DIcons/Hetran/Hetran.SMP-HT.jpg
./2DIcons/Hetran/Hetran.X-HS-1.jpg
./2DIcons/Hetran/Hetran.X-HS-2.jpg
./2DIcons/Hetran/Hetran.X-HT-1.jpg
./2DIcons/Hetran/Hetran.X-HT-2.jpg
./2DIcons/Hierarchy/Hierarchy.BLOCK.jpg
./2DIcons/HTRIXIST/HTRIXIST.BLOCK.jpg
./2DIcons/HTRIXIST/HTRIXIST.E-HS-1CN.jpg
./2DIcons/HTRIXIST/HTRIXIST.E-HS-1CO.jpg
./2DIcons/HTRIXIST/HTRIXIST.E-HS-2.jpg
./2DIcons/HTRIXIST/HTRIXIST.E-HT-1CN.jpg
./2DIcons/HTRIXIST/HTRIXIST.E-HT-1CO.jpg
./2DIcons/HTRIXIST/HTRIXIST.E-HT-2.jpg
./2DIcons/HTRIXIST/HTRIXIST.F-HS-2CN.jpg
./2DIcons/HTRIXIST/HTRIXIST.F-HS-2CO.jpg
./2DIcons/HTRIXIST/HTRIXIST.F-HS-4.jpg
./2DIcons/HTRIXIST/HTRIXIST.F-HT-2CN.jpg
./2DIcons/HTRIXIST/HTRIXIST.F-HT-2CO.jpg
./2DIcons/HTRIXIST/HTRIXIST.F-HT-4.jpg
./2DIcons/HTRIXIST/HTRIXIST.G-HS-2.jpg
./2DIcons/HTRIXIST/HTRIXIST.G-HT-2.jpg
./2DIcons/HTRIXIST/HTRIXIST.GEN-HS.jpg
./2DIcons/HTRIXIST/HTRIXIST.GEN-HT.jpg
./2DIcons/HTRIXIST/HTRIXIST.H-HS-2.jpg
./2DIcons/HTRIXIST/HTRIXIST.H-HT-2.jpg
./2DIcons/HTRIXIST/HTRIXIST.J12-HS1.jpg
./2DIcons/HTRIXIST/HTRIXIST.J12-HS2.jpg
./2DIcons/HTRIXIST/HTRIXIST.J12-HT1.jpg
./2DIcons/HTRIXIST/HTRIXIST.J12-HT2.jpg
./2DIcons/HTRIXIST/HTRIXIST.J21-HS1.jpg
./2DIcons/HTRIXIST/HTRIXIST.J21-HS2.jpg
./2DIcons/HTRIXIST/HTRIXIST.J21-HT1.jpg
./2DIcons/HTRIXIST/HTRIXIST.J21-HT2.jpg
./2DIcons/HTRIXIST/HTRIXIST.K-HT-1.jpg
./2DIcons/HTRIXIST/HTRIXIST.K-HT-2.jpg
./2DIcons/HTRIXIST/HTRIXIST.SIMP-HS.jpg
./2DIcons/HTRIXIST/HTRIXIST.SMP-HT.jpg
./2DIcons/HTRIXIST/HTRIXIST.X-HS-1.jpg
./2DIcons/HTRIXIST/HTRIXIST.X-HS-2.jpg
./2DIcons/HTRIXIST/HTRIXIST.X-HT-1.jpg
./2DIcons/HTRIXIST/HTRIXIST.X-HT-2.jpg
./2DIcons/HXFlux/HXFlux.BLOCK.jpg
./2DIcons/HyCyc/HyCyc.BLOCK.jpg
./2DIcons/HyCyc/HyCyc.ICON.jpg
./2DIcons/MCompr/MCompr.BLOCK.jpg
./2DIcons/MCompr/MCompr.ICON1.jpg
./2DIcons/Measurement/Measurement.ACONTLR.jpg
./2DIcons/Measurement/Measurement.AINDICTR.jpg
./2DIcons/Measurement/Measurement.BLOCK.jpg
./2DIcons/Measurement/Measurement.FCONTLR.jpg
./2DIcons/Measurement/Measurement.FINDICTR.jpg
./2DIcons/Measurement/Measurement.LCONTLR.jpg
./2DIcons/Measurement/Measurement.LINDICTR.jpg
./2DIcons/Measurement/Measurement.MEASURE1.jpg
./2DIcons/Measurement/Measurement.MEASURE2.jpg
./2DIcons/Measurement/Measurement.MEASURE3.jpg
./2DIcons/Measurement/Measurement.MEASURE4.jpg
./2DIcons/Measurement/Measurement.MEASURE5.jpg
./2DIcons/Measurement/Measurement.MEASURE6.jpg
./2DIcons/Measurement/Measurement.MEASURE7.jpg
./2DIcons/Measurement/Measurement.MEASURE8.jpg
./2DIcons/Measurement/Measurement.PCONTLR.jpg
./2DIcons/Measurement/Measurement.PINDICTR.jpg
./2DIcons/Measurement/Measurement.TCONTLR.jpg
./2DIcons/Measurement/Measurement.TINDICTR.jpg
./2DIcons/MHeatX/MHeatX.BLOCK.jpg
./2DIcons/MHeatX/MHeatX.BLOCK2.jpg
./2DIcons/MHeatX/MHeatX.BLOCK3.jpg
./2DIcons/MHeatX/MHeatX.CIRC-MHX.jpg
./2DIcons/MHeatX/MHeatX.COCURNT.jpg
./2DIcons/MHeatX/MHeatX.COUNTER.jpg
./2DIcons/MHeatX/MHeatX.COUNTER2.jpg
./2DIcons/MHeatX/MHeatX.ICON1.jpg
./2DIcons/MHeatX/MHeatX.SIMP-MHX.jpg
./2DIcons/Mixer/Mixer.3WAY.jpg
./2DIcons/Mixer/Mixer.BLOCK.jpg
./2DIcons/Mixer/Mixer.DOT.jpg
./2DIcons/Mixer/Mixer.HEAT.jpg
./2DIcons/Mixer/Mixer.HOPPER.jpg
./2DIcons/Mixer/Mixer.SCREW.jpg
./2DIcons/Mixer/Mixer.TANK.jpg
./2DIcons/Mixer/Mixer.TEE.jpg
./2DIcons/Mixer/Mixer.TRIANGLE.jpg
./2DIcons/Mixer/Mixer.VALVE.jpg
./2DIcons/Mixer/Mixer.WORK.jpg
./2DIcons/Mult/Mult.BLOCK.jpg
./2DIcons/Mult/Mult.DOT.jpg
./2DIcons/Mult/Mult.HEAT.jpg
./2DIcons/Mult/Mult.WORK.jpg
./2DIcons/MultiFrac/MultiFrac.AIRCOL.jpg
./2DIcons/MultiFrac/MultiFrac.BLOCK.jpg
./2DIcons/MultiFrac/MultiFrac.CDU1.jpg
./2DIcons/MultiFrac/MultiFrac.CDU2.jpg
./2DIcons/MultiFrac/MultiFrac.CDU3.jpg
./2DIcons/MultiFrac/MultiFrac.CDU4.jpg
./2DIcons/MultiFrac/MultiFrac.CDU5.jpg
./2DIcons/MultiFrac/MultiFrac.CDU6.jpg
./2DIcons/MultiFrac/MultiFrac.CDU7.jpg
./2DIcons/MultiFrac/MultiFrac.CDU8.jpg
./2DIcons/MultiFrac/MultiFrac.CDU9.jpg
./2DIcons/MultiFrac/MultiFrac.PETLYUK.jpg
./2DIcons/MultiFrac/MultiFrac.PFRAC.jpg
./2DIcons/MultiFrac/MultiFrac.PREFLASH.jpg
./2DIcons/MultiFrac/MultiFrac.VACUUM1.jpg
./2DIcons/MultiFrac/MultiFrac.VACUUM2.jpg
./2DIcons/PetroFrac/PetroFrac.ABSBR.jpg
./2DIcons/PetroFrac/PetroFrac.BLOCK.jpg
./2DIcons/PetroFrac/PetroFrac.CDU1.jpg
./2DIcons/PetroFrac/PetroFrac.CDU10.jpg
./2DIcons/PetroFrac/PetroFrac.CDU10F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU11.jpg
./2DIcons/PetroFrac/PetroFrac.CDU11F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU12.jpg
./2DIcons/PetroFrac/PetroFrac.CDU12F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU13.jpg
./2DIcons/PetroFrac/PetroFrac.CDU13F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU14.jpg
./2DIcons/PetroFrac/PetroFrac.CDU14F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU15.jpg
./2DIcons/PetroFrac/PetroFrac.CDU15F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU1F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU2.jpg
./2DIcons/PetroFrac/PetroFrac.CDU2F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU3.jpg
./2DIcons/PetroFrac/PetroFrac.CDU3F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU4.jpg
./2DIcons/PetroFrac/PetroFrac.CDU4F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU5.jpg
./2DIcons/PetroFrac/PetroFrac.CDU5F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU6.jpg
./2DIcons/PetroFrac/PetroFrac.CDU6F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU7.jpg
./2DIcons/PetroFrac/PetroFrac.CDU7F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU8.jpg
./2DIcons/PetroFrac/PetroFrac.CDU8F.jpg
./2DIcons/PetroFrac/PetroFrac.CDU9.jpg
./2DIcons/PetroFrac/PetroFrac.CDU9F.jpg
./2DIcons/PetroFrac/PetroFrac.FCC-MF1.jpg
./2DIcons/PetroFrac/PetroFrac.FCC-MF2.jpg
./2DIcons/PetroFrac/PetroFrac.FRACT.jpg
./2DIcons/PetroFrac/PetroFrac.PFRAC.jpg
./2DIcons/PetroFrac/PetroFrac.PFRACF.jpg
./2DIcons/PetroFrac/PetroFrac.PREFL1.jpg
./2DIcons/PetroFrac/PetroFrac.PREFL1F.jpg
./2DIcons/PetroFrac/PetroFrac.PREFL2.jpg
./2DIcons/PetroFrac/PetroFrac.PREFL2F.jpg
./2DIcons/PetroFrac/PetroFrac.STRIP.jpg
./2DIcons/PetroFrac/PetroFrac.VACUUM1.jpg
./2DIcons/PetroFrac/PetroFrac.VACUUM1F.jpg
./2DIcons/PetroFrac/PetroFrac.VACUUM2.jpg
./2DIcons/PetroFrac/PetroFrac.VACUUM2F.jpg
./2DIcons/Pipe/Pipe.BLOCK.jpg
./2DIcons/Pipe/Pipe.D-PIPE.jpg
./2DIcons/Pipe/Pipe.HI-PIPE.jpg
./2DIcons/Pipe/Pipe.U-PIPE.jpg
./2DIcons/Pipe/Pipe.V-PIPE.jpg
./2DIcons/Pipeline/Pipeline.BLOCK.jpg
./2DIcons/Pipeline/Pipeline.D-PIPE.jpg
./2DIcons/Pipeline/Pipeline.HI-PIPE.jpg
./2DIcons/Pipeline/Pipeline.U-PIPE.jpg
./2DIcons/Pipeline/Pipeline.V-PIPE.jpg
./2DIcons/Pump/Pump.BLOCK.jpg
./2DIcons/Pump/Pump.ICON1.jpg
./2DIcons/Pump/Pump.ICON2.jpg
./2DIcons/Qtvec/Qtvec.BLOCK.jpg
./2DIcons/Qtvec/Qtvec.DOT.jpg
./2DIcons/RadFrac/RadFrac.ABSBR1.jpg
./2DIcons/RadFrac/RadFrac.ABSBR2.jpg
./2DIcons/RadFrac/RadFrac.ABSBR3.jpg
./2DIcons/RadFrac/RadFrac.BLOCK.jpg
./2DIcons/RadFrac/RadFrac.DECANT1.jpg
./2DIcons/RadFrac/RadFrac.DECANT2.jpg
./2DIcons/RadFrac/RadFrac.DECANT3.jpg
./2DIcons/RadFrac/RadFrac.FRACT1.jpg
./2DIcons/RadFrac/RadFrac.FRACT2.jpg
./2DIcons/RadFrac/RadFrac.PACKABS.jpg
./2DIcons/RadFrac/RadFrac.PACKCOL1.jpg
./2DIcons/RadFrac/RadFrac.PACKCOL2.jpg
./2DIcons/RadFrac/RadFrac.PACKSTR1.jpg
./2DIcons/RadFrac/RadFrac.PACKSTR2.jpg
./2DIcons/RadFrac/RadFrac.RECT.jpg
./2DIcons/RadFrac/RadFrac.STRIP1.jpg
./2DIcons/RadFrac/RadFrac.STRIP2.jpg
./2DIcons/RateFrac/RateFrac.ABSBR2.jpg
./2DIcons/RateFrac/RateFrac.ABSBR3.jpg
./2DIcons/RateFrac/RateFrac.ABSORBER.jpg
./2DIcons/RateFrac/RateFrac.BLOCK.jpg
./2DIcons/RateFrac/RateFrac.FRACT.jpg
./2DIcons/RateFrac/RateFrac.PACKABS.jpg
./2DIcons/RateFrac/RateFrac.PACKCOL.jpg
./2DIcons/RateFrac/RateFrac.PACKSTR.jpg
./2DIcons/RateFrac/RateFrac.RECT.jpg
./2DIcons/RateFrac/RateFrac.STRIPPER.jpg
./2DIcons/RateFrac/RateFrac.VACUUM1.jpg
./2DIcons/RateFrac/RateFrac.VACUUM2.jpg
./2DIcons/RBatch/RBatch.BLOCK.jpg
./2DIcons/RBatch/RBatch.ICON1.jpg
./2DIcons/RCSTR/RCSTR.BLOCK.jpg
./2DIcons/RCSTR/RCSTR.ICON1.jpg
./2DIcons/REquil/REquil.BLOCK.jpg
./2DIcons/REquil/REquil.ICON2.jpg
./2DIcons/REquil/REquil.ICON3.jpg
./2DIcons/RGibbs/RGibbs.BLOCK.jpg
./2DIcons/RGibbs/RGibbs.ICON1.jpg
./2DIcons/RGibbs/RGibbs.ICON2.jpg
./2DIcons/RPlug/RPlug.BLOCK.jpg
./2DIcons/RPlug/RPlug.ICON1.jpg
./2DIcons/RPlug/RPlug.ICON2.jpg
./2DIcons/RPlug/RPlug.ICON3.jpg
./2DIcons/RStoic/RStoic.BLOCK.jpg
./2DIcons/RStoic/RStoic.ICON1.jpg
./2DIcons/RStoic/RStoic.ICON2.jpg
./2DIcons/RStoic/RStoic.ICON3.jpg
./2DIcons/RStoic/RStoic.ICON4.jpg
./2DIcons/RYield/RYield.BLOCK.jpg
./2DIcons/RYield/RYield.ICON2.jpg
./2DIcons/RYield/RYield.ICON3.jpg
./2DIcons/SCFrac/SCFrac.BLOCK.jpg
./2DIcons/SCFrac/SCFrac.CDU1.jpg
./2DIcons/SCFrac/SCFrac.CDU2.jpg
./2DIcons/SCFrac/SCFrac.CDU3.jpg
./2DIcons/SCFrac/SCFrac.VACUUM1.jpg
./2DIcons/SCFrac/SCFrac.VACUUM2.jpg
./2DIcons/Screen/Screen.BLOCK.jpg
./2DIcons/Screen/Screen.ICON1.jpg
./2DIcons/Screen/Screen.ICON2.jpg
./2DIcons/Selector/Selector.BLOCK.jpg
./2DIcons/Selector/Selector.HEAT.jpg
./2DIcons/Selector/Selector.TRIANGLE.jpg
./2DIcons/Selector/Selector.WORK.jpg
./2DIcons/Sep/Sep.BLOCK.jpg
./2DIcons/Sep/Sep.ICON1.jpg
./2DIcons/Sep/Sep.ICON2.jpg
./2DIcons/Sep/Sep.ICON3.jpg
./2DIcons/Sep2/Sep2.BLOCK.jpg
./2DIcons/Sep2/Sep2.ICON1.jpg
./2DIcons/Sep2/Sep2.ICON2.jpg
./2DIcons/Sep2/Sep2.ICON3.jpg
./2DIcons/SSplit/SSplit.3WAY.jpg
./2DIcons/SSplit/SSplit.BLOCK.jpg
./2DIcons/SSplit/SSplit.CCD.jpg
./2DIcons/SSplit/SSplit.CFUGE.jpg
./2DIcons/SSplit/SSplit.CYCLONE.jpg
./2DIcons/SSplit/SSplit.DOT.jpg
./2DIcons/SSplit/SSplit.FILTER1.jpg
./2DIcons/SSplit/SSplit.FILTER2.jpg
./2DIcons/SSplit/SSplit.SCREEN.jpg
./2DIcons/SSplit/SSplit.TEE.jpg
./2DIcons/SSplit/SSplit.TRIANGLE.jpg
./2DIcons/SSplit/SSplit.VSCRUB.jpg
./2DIcons/SWash/SWash.BLOCK.jpg
./2DIcons/SWash/SWash.ICON.jpg
./2DIcons/User/User.BLOCK.jpg
./2DIcons/User/User.SMALL.jpg
./2DIcons/User2/User2.BLOCK.jpg
./2DIcons/User2/User2.CFUGE.jpg
./2DIcons/User2/User2.CSTR.jpg
./2DIcons/User2/User2.EXCEL.jpg
./2DIcons/User2/User2.FILTER.jpg
./2DIcons/User2/User2.FRACT.jpg
./2DIcons/User2/User2.H-DRUM.jpg
./2DIcons/User2/User2.HEATER.jpg
./2DIcons/User2/User2.HEATX1.jpg
./2DIcons/User2/User2.HEATX2.jpg
./2DIcons/User2/User2.PLUG.jpg
./2DIcons/User2/User2.REACTOR.jpg
./2DIcons/User2/User2.RECT.jpg
./2DIcons/User2/User2.STRIP.jpg
./2DIcons/User2/User2.V-DRUM.jpg
./2DIcons/User2/User2.VALVE4.jpg
./2DIcons/User3/User3.BLOCK.jpg
./2DIcons/User3/User3.CFUGE.jpg
./2DIcons/User3/User3.CSTR.jpg
./2DIcons/User3/User3.FILTER.jpg
./2DIcons/User3/User3.FRACT.jpg
./2DIcons/User3/User3.H-DRUM.jpg
./2DIcons/User3/User3.HEATER.jpg
./2DIcons/User3/User3.HEATX1.jpg
./2DIcons/User3/User3.HEATX2.jpg
./2DIcons/User3/User3.PLUG.jpg
./2DIcons/User3/User3.REACTOR.jpg
./2DIcons/User3/User3.RECT.jpg
./2DIcons/User3/User3.STRIP.jpg
./2DIcons/User3/User3.V-DRUM.jpg
./2DIcons/User3/User3.VALVE4.jpg
./2DIcons/Valve/Valve.BLOCK.jpg
./2DIcons/Valve/Valve.VALVE1.jpg
./2DIcons/Valve/Valve.VALVE2.jpg
./2DIcons/Valve/Valve.VALVE3.jpg
./2DIcons/Valve/Valve.VALVE4.jpg
./2DIcons/VScrub/VScrub.BLOCK.jpg
./2DIcons/VScrub/VScrub.ICON.jpg*/
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_ECN_HS_1();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_ECN_HS_2();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_ECN_HT_1();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_ECN_HT_2();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_FORCED_1();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_FORCED_2();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_INDUCE_1();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_INDUCE_2();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_NATURA_1();
    tempIconMap[ " " ] = GetVESuite_Aerotran_Aerotran_NATURA_2();
    tempIconMap[ " " ] = GetVESuite_Analyzer_Analyzer_ANALYZE2();
    tempIconMap[ " " ] = GetVESuite_Analyzer_Analyzer_ANALYZER();
    tempIconMap[ " " ] = GetVESuite_Analyzer_Analyzer_BLOCK();
    tempIconMap[ " " ] = GetVESuite_BatchFrac_BatchFrac_BLOCK();
    tempIconMap[ " " ] = GetVESuite_BatchFrac_BatchFrac_COLUMN1();
    tempIconMap[ " " ] = GetVESuite_BatchFrac_BatchFrac_COLUMN2();
    tempIconMap[ " " ] = GetVESuite_BatchFrac_BatchFrac_COLUMN3();
    tempIconMap[ " " ] = GetVESuite_CCD_CCD_BLOCK();
    tempIconMap[ " " ] = GetVESuite_CCD_CCD_ICON();
    tempIconMap[ " " ] = GetVESuite_CFuge_CFuge_BLOCK();
    tempIconMap[ " " ] = GetVESuite_CFuge_CFuge_ICON();
    tempIconMap[ " " ] = GetVESuite_ClChng_ClChng_BLOCK();
    tempIconMap[ " " ] = GetVESuite_ClChng_ClChng_ICON1();
    tempIconMap[ " " ] = GetVESuite_Compr_Compr_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Compr_Compr_ICON1();
    tempIconMap[ " " ] = GetVESuite_Compr_Compr_ICON2();
    tempIconMap[ " " ] = GetVESuite_Compr_Compr_ICON3();
    tempIconMap[ " " ] = GetVESuite_Crusher_Crusher_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Crusher_Crusher_ICON();
    tempIconMap[ " " ] = GetVESuite_Crystallizer_Crystallizer_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Crystallizer_Crystallizer_ICON1();
    tempIconMap[ " " ] = GetVESuite_Crystallizer_Crystallizer_ICON2();
    tempIconMap[ " " ] = GetVESuite_Cyclone_Cyclone_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Cyclone_Cyclone_ICON();
    tempIconMap[ " " ] = GetVESuite_Decanter_Decanter_V_DRUM();
    tempIconMap[ " " ] = GetVESuite_Decanter_Decanter_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Decanter_Decanter_H_DRUM();
    tempIconMap[ " " ] = GetVESuite_Distl_Distl_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Distl_Distl_ICON1();
    tempIconMap[ " " ] = GetVESuite_Distl_Distl_ICON2();
    tempIconMap[ " " ] = GetVESuite_Dryer_Dryer_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Dryer_Dryer_FLUIDBED();
    tempIconMap[ " " ] = GetVESuite_Dryer_Dryer_FLUIDBED2();
    tempIconMap[ " " ] = GetVESuite_Dryer_Dryer_SPRAY();
    tempIconMap[ " " ] = GetVESuite_DSTWU_DSTWU_BLOCK();
    tempIconMap[ " " ] = GetVESuite_DSTWU_DSTWU_ICON1();
    tempIconMap[ " " ] = GetVESuite_DSTWU_DSTWU_ICON2();
    tempIconMap[ " " ] = GetVESuite_Dupl_Dupl_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Dupl_Dupl_DOT();
    tempIconMap[ " " ] = GetVESuite_Dupl_Dupl_HEAT();
    tempIconMap[ " " ] = GetVESuite_Dupl_Dupl_WORK();
    tempIconMap[ " " ] = GetVESuite_ESP_ESP_BLOCK();
    tempIconMap[ " " ] = GetVESuite_ESP_ESP_ICON();
    tempIconMap[ " " ] = GetVESuite_Extract_Extract_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Extract_Extract_ICON1();
    tempIconMap[ " " ] = GetVESuite_Extract_Extract_ICON2();
    tempIconMap[ " " ] = GetVESuite_Extract_Extract_POD();
    tempIconMap[ " " ] = GetVESuite_FabFl_FabFl_BLOCK();
    tempIconMap[ " " ] = GetVESuite_FabFl_FabFl_ICON();
    tempIconMap[ " " ] = GetVESuite_Feedbl_Feedbl_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Feedbl_Feedbl_FEEDBL();
    tempIconMap[ " " ] = GetVESuite_Filter_Filter_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Filter_Filter_PLATE();
    tempIconMap[ " " ] = GetVESuite_Filter_Filter_ROTARY();
    tempIconMap[ " " ] = GetVESuite_Flash2_Flash2_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Flash2_Flash2_FURNACE();
    tempIconMap[ " " ] = GetVESuite_Flash2_Flash2_H_DRUM();
    tempIconMap[ " " ] = GetVESuite_Flash2_Flash2_HEATER();
    tempIconMap[ " " ] = GetVESuite_Flash2_Flash2_V_DRUM1();
    tempIconMap[ " " ] = GetVESuite_Flash2_Flash2_V_DRUM2();
    tempIconMap[ " " ] = GetVESuite_Flash3_Flash3_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Flash3_Flash3_FURNACE();
    tempIconMap[ " " ] = GetVESuite_Flash3_Flash3_H_DRUM();
    tempIconMap[ " " ] = GetVESuite_Flash3_Flash3_HEATER();
    tempIconMap[ " " ] = GetVESuite_Flash3_Flash3_V_DRUM1();
    tempIconMap[ " " ] = GetVESuite_Flash3_Flash3_V_DRUM2();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_3WAY();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_BLOCK();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_DOT();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_HEAT_TEE();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_HEAT();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_TEE();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_TRIANGLE();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_WORK_TEE();
    tempIconMap[ " " ] = GetVESuite_FSplit_FSplit_WORK();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_AIRCOOLER();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_COMPR();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_FURNACE();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_Heater();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_PUMP();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_VALVE();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_VALVE2();
    tempIconMap[ " " ] = GetVESuite_Heater_Heater_VALVE4();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_BLOCK();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_E_HS_1CN();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_E_HS_1CO();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_E_HS_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_E_HT_1CN();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_E_HT_1CO();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_E_HT_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_ECN_HS_1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_ECN_HS_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_ECN_HT_1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_ECN_HT_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_F_HS_2CN();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_F_HS_2CO();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_F_HS_4();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_F_HT_2CN();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_F_HT_2CO();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_F_HT_4();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_FORCED_1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_FORCED_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_G_HS_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_G_HT_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_GEN_HS();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_GEN_HT();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_H_HS_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_H_HT_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_INDUCE_1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_INDUCE_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_J12_HS1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_J12_HS2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_J12_HT1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_J12_HT2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_J21_HS1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_J21_HS2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_J21_HT1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_J21_HT2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_K_HT_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_NATURA_1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_NATURA_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_SIMP_HS();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_SIMP_HT();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_X_HS_1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_X_HS_2();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_X_HT_1();
    tempIconMap[ " " ] = GetVESuite_HeatX_HeatX_X_HT_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_E_HS_1CN();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_E_HS_1CO();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_E_HS_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_E_HT_1CN();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_E_HT_1CO();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_E_HT_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_F_HS_2CN();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_F_HS_2CO();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_F_HS_4();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_F_HT_2CN();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_F_HT_2CO();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_F_HT_4();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_G_HS_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_G_HT_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_GEN_HS();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_GEN_HT();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_H_HS_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_H_HT_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_J12_HS1();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_J12_HS2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_J12_HT1();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_J12_HT2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_J21_HS1();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_J21_HS2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_J21_HT1();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_J21_HT2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_K_HT_1();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_K_HT_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_SIMP_HS();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_SMP_HT();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_X_HS_1();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_X_HS_2();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_X_HT_1();
    tempIconMap[ " " ] = GetVESuite_Hetran_Hetran_X_HT_2();
    tempIconMap[ " " ] = GetVESuite_Hierarchy_Hierarchy_BLOCK();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_BLOCK();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_E_HS_1CN();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_E_HS_1CO();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_E_HS_2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_E_HT_1CN();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_E_HT_1CO();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_E_HT_2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_F_HS_2CN();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_F_HS_2CO();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_F_HS_4();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_F_HT_2CN();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_F_HT_2CO();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_F_HT_4();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_G_HS_2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_G_HT_2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_GEN_HS();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_GEN_HT();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_H_HS_2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_H_HT_2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_J12_HS1();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_J12_HS2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_J12_HT1();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_J12_HT2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_J21_HS1();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_J21_HS2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_J21_HT1();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_J21_HT2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_K_HT_1();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_K_HT_2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_SIMP_HS();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_SMP_HT();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_X_HS_1();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_X_HS_2();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_X_HT_1();
    tempIconMap[ " " ] = GetVESuite_HTRIXIST_HTRIXIST_X_HT_2();
    tempIconMap[ " " ] = GetVESuite_HXFlux_HXFlux_BLOCK();
    tempIconMap[ " " ] = GetVESuite_HyCyc_HyCyc_BLOCK();
    tempIconMap[ " " ] = GetVESuite_HyCyc_HyCyc_ICON();
    tempIconMap[ " " ] = GetVESuite_MCompr_MCompr_BLOCK();
    tempIconMap[ " " ] = GetVESuite_MCompr_MCompr_ICON1();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_ACONTLR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_AINDICTR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_FCONTLR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_FINDICTR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_LCONTLR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_LINDICTR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_MEASURE1();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_MEASURE2();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_MEASURE3();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_MEASURE4();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_MEASURE5();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_MEASURE6();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_MEASURE7();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_MEASURE8();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_PCONTLR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_PINDICTR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_TCONTLR();
    tempIconMap[ " " ] = GetVESuite_Measurement_Measurement_TINDICTR();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_BLOCK();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_BLOCK2();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_BLOCK3();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_CIRC_MHX();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_COCURNT();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_COUNTER();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_COUNTER2();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_ICON1();
    tempIconMap[ " " ] = GetVESuite_MHeatX_MHeatX_SIMP_MHX();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_3WAY();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_DOT();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_HEAT();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_HOPPER();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_SCREW();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_TANK();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_TEE();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_TRIANGLE();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_VALVE();
    tempIconMap[ " " ] = GetVESuite_Mixer_Mixer_WORK();
    tempIconMap[ " " ] = GetVESuite_Mult_Mult_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Mult_Mult_DOT();
    tempIconMap[ " " ] = GetVESuite_Mult_Mult_HEAT();
    tempIconMap[ " " ] = GetVESuite_Mult_Mult_WORK();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_AIRCOL();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_BLOCK();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU1();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU2();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU3();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU4();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU5();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU6();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU7();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU8();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_CDU9();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_PETLYUK();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_PFRAC();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_PREFLASH();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_VACUUM1();
    tempIconMap[ " " ] = GetVESuite_MultiFrac_MultiFrac_VACUUM2();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_ABSBR();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_BLOCK();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU1();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU10();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU10F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU11();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU11F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU12();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU12F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU13();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU13F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU14();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU14F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU15();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU15F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU1F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU2();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU2F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU3();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU3F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU4();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU4F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU5();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU5F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU6();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU6F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU7();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU7F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU8();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU8F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU9();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_CDU9F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_FCC_MF1();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_FCC_MF2();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_FRACT();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_PFRAC();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_PFRACF();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_PREFL1();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_PREFL1F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_PREFL2();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_PREFL2F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_STRIP();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_VACUUM1();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_VACUUM1F();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_VACUUM2();
    tempIconMap[ " " ] = GetVESuite_PetroFrac_PetroFrac_VACUUM2F();
    tempIconMap[ " " ] = GetVESuite_Pipe_Pipe_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Pipe_Pipe_D_PIPE();
    tempIconMap[ " " ] = GetVESuite_Pipe_Pipe_HI_PIPE();
    tempIconMap[ " " ] = GetVESuite_Pipe_Pipe_U_PIPE();
    tempIconMap[ " " ] = GetVESuite_Pipe_Pipe_V_PIPE();
    tempIconMap[ " " ] = GetVESuite_Pipeline_Pipeline_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Pipeline_Pipeline_D_PIPE();
    tempIconMap[ " " ] = GetVESuite_Pipeline_Pipeline_HI_PIPE();
    tempIconMap[ " " ] = GetVESuite_Pipeline_Pipeline_U_PIPE();
    tempIconMap[ " " ] = GetVESuite_Pipeline_Pipeline_V_PIPE();
    tempIconMap[ " " ] = GetVESuite_Pump_Pump_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Pump_Pump_ICON1();
    tempIconMap[ " " ] = GetVESuite_Pump_Pump_ICON2();
    tempIconMap[ " " ] = GetVESuite_Qtvec_Qtvec_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Qtvec_Qtvec_DOT();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_ABSBR1();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_ABSBR2();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_ABSBR3();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_BLOCK();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_DECANT1();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_DECANT2();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_DECANT3();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_FRACT1();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_FRACT2();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_PACKABS();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_PACKCOL1();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_PACKCOL2();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_PACKSTR1();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_PACKSTR2();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_RECT();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_STRIP1();
    tempIconMap[ " " ] = GetVESuite_RadFrac_RadFrac_STRIP2();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_ABSBR2();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_ABSBR3();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_ABSORBER();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_BLOCK();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_FRACT();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_PACKABS();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_PACKCOL();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_PACKSTR();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_RECT();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_STRIPPER();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_VACUUM1();
    tempIconMap[ " " ] = GetVESuite_RateFrac_RateFrac_VACUUM2();
    tempIconMap[ " " ] = GetVESuite_RBatch_RBatch_BLOCK();
    tempIconMap[ " " ] = GetVESuite_RBatch_RBatch_ICON1();
    tempIconMap[ " " ] = GetVESuite_RCSTR_RCSTR_BLOCK();
    tempIconMap[ " " ] = GetVESuite_RCSTR_RCSTR_ICON1();
    tempIconMap[ " " ] = GetVESuite_REquil_REquil_BLOCK();
    tempIconMap[ " " ] = GetVESuite_REquil_REquil_ICON2();
    tempIconMap[ " " ] = GetVESuite_REquil_REquil_ICON3();
    tempIconMap[ " " ] = GetVESuite_RGibbs_RGibbs_BLOCK();
    tempIconMap[ " " ] = GetVESuite_RGibbs_RGibbs_ICON1();
    tempIconMap[ " " ] = GetVESuite_RGibbs_RGibbs_ICON2();
    tempIconMap[ " " ] = GetVESuite_RPlug_RPlug_BLOCK();
    tempIconMap[ " " ] = GetVESuite_RPlug_RPlug_ICON1();
    tempIconMap[ " " ] = GetVESuite_RPlug_RPlug_ICON2();
    tempIconMap[ " " ] = GetVESuite_RPlug_RPlug_ICON3();
    tempIconMap[ " " ] = GetVESuite_RStoic_RStoic_BLOCK();
    tempIconMap[ " " ] = GetVESuite_RStoic_RStoic_ICON1();
    tempIconMap[ " " ] = GetVESuite_RStoic_RStoic_ICON2();
    tempIconMap[ " " ] = GetVESuite_RStoic_RStoic_ICON3();
    tempIconMap[ " " ] = GetVESuite_RStoic_RStoic_ICON4();
    tempIconMap[ " " ] = GetVESuite_RYield_RYield_BLOCK();
    tempIconMap[ " " ] = GetVESuite_RYield_RYield_ICON2();
    tempIconMap[ " " ] = GetVESuite_RYield_RYield_ICON3();
    tempIconMap[ " " ] = GetVESuite_SCFrac_SCFrac_BLOCK();
    tempIconMap[ " " ] = GetVESuite_SCFrac_SCFrac_CDU1();
    tempIconMap[ " " ] = GetVESuite_SCFrac_SCFrac_CDU2();
    tempIconMap[ " " ] = GetVESuite_SCFrac_SCFrac_CDU3();
    tempIconMap[ " " ] = GetVESuite_SCFrac_SCFrac_VACUUM1();
    tempIconMap[ " " ] = GetVESuite_SCFrac_SCFrac_VACUUM2();
    tempIconMap[ " " ] = GetVESuite_Screen_Screen_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Screen_Screen_ICON1();
    tempIconMap[ " " ] = GetVESuite_Screen_Screen_ICON2();
    tempIconMap[ " " ] = GetVESuite_Selector_Selector_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Selector_Selector_HEAT();
    tempIconMap[ " " ] = GetVESuite_Selector_Selector_TRIANGLE();
    tempIconMap[ " " ] = GetVESuite_Selector_Selector_WORK();
    tempIconMap[ " " ] = GetVESuite_Sep2_Sep2_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Sep2_Sep2_ICON1();
    tempIconMap[ " " ] = GetVESuite_Sep2_Sep2_ICON2();
    tempIconMap[ " " ] = GetVESuite_Sep2_Sep2_ICON3();
    tempIconMap[ " " ] = GetVESuite_Sep_Sep_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Sep_Sep_ICON1();
    tempIconMap[ " " ] = GetVESuite_Sep_Sep_ICON2();
    tempIconMap[ " " ] = GetVESuite_Sep_Sep_ICON3();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_3WAY();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_BLOCK();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_CCD();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_CFUGE();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_CYCLONE();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_DOT();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_FILTER1();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_FILTER2();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_SCREEN();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_TEE();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_TRIANGLE();
    tempIconMap[ " " ] = GetVESuite_SSplit_SSplit_VSCRUB();
    tempIconMap[ " " ] = GetVESuite_SWash_SWash_BLOCK();
    tempIconMap[ " " ] = GetVESuite_SWash_SWash_ICON();
    tempIconMap[ " " ] = GetVESuite_User2_User2_BLOCK();
    tempIconMap[ " " ] = GetVESuite_User2_User2_CFUGE();
    tempIconMap[ " " ] = GetVESuite_User2_User2_CSTR();
    tempIconMap[ " " ] = GetVESuite_User2_User2_EXCEL();
    tempIconMap[ " " ] = GetVESuite_User2_User2_FILTER();
    tempIconMap[ " " ] = GetVESuite_User2_User2_FRACT();
    tempIconMap[ " " ] = GetVESuite_User2_User2_H_DRUM();
    tempIconMap[ " " ] = GetVESuite_User2_User2_HEATER();
    tempIconMap[ " " ] = GetVESuite_User2_User2_HEATX1();
    tempIconMap[ " " ] = GetVESuite_User2_User2_HEATX2();
    tempIconMap[ " " ] = GetVESuite_User2_User2_PLUG();
    tempIconMap[ " " ] = GetVESuite_User2_User2_REACTOR();
    tempIconMap[ " " ] = GetVESuite_User2_User2_RECT();
    tempIconMap[ " " ] = GetVESuite_User2_User2_STRIP();
    tempIconMap[ " " ] = GetVESuite_User2_User2_V_DRUM();
    tempIconMap[ " " ] = GetVESuite_User2_User2_VALVE4();
    tempIconMap[ " " ] = GetVESuite_User3_User3_BLOCK();
    tempIconMap[ " " ] = GetVESuite_User3_User3_CFUGE();
    tempIconMap[ " " ] = GetVESuite_User3_User3_CSTR();
    tempIconMap[ " " ] = GetVESuite_User3_User3_FILTER();
    tempIconMap[ " " ] = GetVESuite_User3_User3_FRACT();
    tempIconMap[ " " ] = GetVESuite_User3_User3_H_DRUM();
    tempIconMap[ " " ] = GetVESuite_User3_User3_HEATER();
    tempIconMap[ " " ] = GetVESuite_User3_User3_HEATX1();
    tempIconMap[ " " ] = GetVESuite_User3_User3_HEATX2();
    tempIconMap[ " " ] = GetVESuite_User3_User3_PLUG();
    tempIconMap[ " " ] = GetVESuite_User3_User3_REACTOR();
    tempIconMap[ " " ] = GetVESuite_User3_User3_RECT();
    tempIconMap[ " " ] = GetVESuite_User3_User3_STRIP();
    tempIconMap[ " " ] = GetVESuite_User3_User3_V_DRUM();
    tempIconMap[ " " ] = GetVESuite_User3_User3_VALVE4();
    tempIconMap[ " " ] = GetVESuite_User_User_BLOCK();
    tempIconMap[ " " ] = GetVESuite_User_User_SMALL();
    tempIconMap[ " " ] = GetVESuite_Valve_Valve_BLOCK();
    tempIconMap[ " " ] = GetVESuite_Valve_Valve_VALVE1();
    tempIconMap[ " " ] = GetVESuite_Valve_Valve_VALVE2();
    tempIconMap[ " " ] = GetVESuite_Valve_Valve_VALVE3();
    tempIconMap[ " " ] = GetVESuite_Valve_Valve_VALVE4();
    tempIconMap[ " " ] = GetVESuite_VScrub_VScrub_BLOCK();
    tempIconMap[ " " ] = GetVESuite_VScrub_VScrub_ICON();
    return tempIconMap;
}