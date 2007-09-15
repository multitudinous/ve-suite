/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
#include "VE_Conductor/GUIPlugin/IconChooser.h"
#include <wx/dir.h>
#include <wx/image.h>
#include "VE_Conductor/GUIPlugin/UIPluginBase.h"

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

BEGIN_EVENT_TABLE(IconChooser,wxFrame)	
	EVT_CLOSE(IconChooser::OnClose)
	EVT_BUTTON(1003,IconChooser::okButtonClick)
	EVT_BUTTON(1004,IconChooser::cancelButtonClick)
	EVT_MENU(1005, IconChooser::IconDirectoryClick)
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////////////////////
IconChooser::IconChooser(wxWindow *parent, /*std::string path,*/ wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxFrame(parent, id, title, position, size, style)
{
	//directory = wxString(path.c_str(), wxConvUTF8);
	CreateGUIControls();
	networkFrame = parent;
}
////////////////////////////////////////////////////////////////////////////////
IconChooser::~IconChooser()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::CreateGUIControls()
{
	WxPanel = new wxPanel(this, 1000, wxPoint(0,0), wxSize(640,480));
	WxNotebook = new wxNotebook(WxPanel, 1001, wxPoint(0,0),wxSize(617,460));
	WxEdit = new wxTextCtrl(WxPanel, 1002, wxT(""), wxPoint(10, 463), wxSize(300,21), 0, wxDefaultValidator, wxT(""));

	choices.Add( _("None") );
	choices.Add( _("Rotate Left") );
	choices.Add( _("Rotate Right") );
	choices.Add( _("Flip Left/Right") );
	choices.Add( _("Flip Up/Down") );
	WxChoice = new wxChoice(WxPanel, 1006, wxPoint(325, 463), wxSize(100,21), choices, 0, wxDefaultValidator, wxT("WxChoice"));
    WxChoice->Select( 0 );

	WxEdit->SetEditable(false);
	okButton = new wxButton(WxPanel, 1003, wxT("OK"), wxPoint(450, 463));
	cancelButton = new wxButton(WxPanel, 1004, wxT("Cancel"), wxPoint(535, 463));
	//WxChoice = new wxChoice(WxPanel, 1003, wxPoint(220,3), wxSize(200,21), componentList, 0, wxDefaultValidator, wxT("Components"));
	//WxChoice->SetSelection(-1);

   {
	WxMenuBar1 = new wxMenuBar();
	wxMenu * AddMenu = new wxMenu(0);
	AddMenu->Append(1005, wxT("Icon Directory"), wxT(""), wxITEM_NORMAL);
	WxMenuBar1->Append(AddMenu, wxT("Add"));
	SetMenuBar(WxMenuBar1);

      int buttonCount = 4000;
      std::vector< wxImage > defaultIcons;
      defaultIcons.push_back( wxImage( contour_xpm ) );
      iconPaths[buttonCount] = "contour.xpm";
      //defaultIcons.push_back( wxImage( cad_tree_selected_xpm ) );
      //iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );
      //defaultIcons.push_back( wxImage( cad_tree_unselected_xpm ) );
      //iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );
      //defaultIcons.push_back( wxImage( cspline_xpm ) );
      //iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );
      defaultIcons.push_back( wxImage( isosurface_xpm ) );
      iconPaths[buttonCount+1] = "isosurface.xpm";
      defaultIcons.push_back( wxImage( ROItb_xpm ) );
      iconPaths[buttonCount+2] = "isosurface.xpm";
      //defaultIcons.push_back( wxImage( square_xpm ) );
      //iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );
      defaultIcons.push_back( wxImage( streamlines_xpm ) );
      iconPaths[buttonCount+3] = "streamlines.xpm";
      defaultIcons.push_back( wxImage( vector_xpm ) );
      iconPaths[buttonCount+4] = "vector.xpm";
      defaultIcons.push_back( wxImage( vectortb_xpm ) );
      iconPaths[buttonCount+5] = "vectortb.xpm";

      wxPanel* WxNoteBookPage = new wxPanel(WxNotebook);
      WxNotebook->AddPage(WxNoteBookPage, wxString( _("Default Icons") ) );
      
      int hCount = 0;
      int vCount = 0;
      int xLoc = 0;
      int yLoc = 0;

      for ( size_t i = 0; i < defaultIcons.size(); ++i )
      {
         //place the button and its label on the current page
         xLoc = 60 * hCount;
         yLoc = 80 * vCount;
         //yLoc = 95 * vCount;
         wxBitmapButton * tempButton = new wxBitmapButton(WxNoteBookPage, buttonCount, defaultIcons.at( i ), wxPoint(xLoc, yLoc));
         tempButton->SetToolTip( _("Default Icon") );
         Connect(buttonCount, wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler(IconChooser::WxButtonClick));
         //wxStaticText * iconLabel = new wxStaticText(WxNoteBookPage, 9999, filename, wxPoint(xLoc, yLoc + 80), wxDefaultSize, 0, filename);
         buttonCount++;
         hCount++;
         //set how many buttons can be placed horizonatally
         //currently 10 buttons
         if(hCount == 10)
         {
            hCount = 0;
            vCount ++;
         }
      }
   }

	SetTitle(wxT("VE Icon Chooser"));
	SetIcon(wxNullIcon);
	SetSize(8,8,630,550);
	Center();
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::OnClose(wxCloseEvent& event)
{
	Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::WxButtonClick(wxCommandEvent& event)
{
    //wxString id;
    //id.Printf("%d", event.GetId());
	//WxEdit->SetValue(id);
	WxEdit->SetValue( wxString( iconPaths[event.GetId()].c_str(), wxConvUTF8 ) );
	//thePlugin->SetImageIcon(iconPaths[event.GetId()]);
}
////////////////////////////////////////////////////////////////////////////////
//void IconChooser::AppendList(const char * input)
//{
//	WxChoice->Append(wxString(input,wxConvUTF8));
//}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::SetPlugin( UIPluginBase * plugin)
{
	thePlugin = plugin;
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::okButtonClick(wxCommandEvent& event)
{
	if(choices[WxChoice->GetCurrentSelection()] == _("None") )
		thePlugin->SetImageIcon( ConvertUnicode( WxEdit->GetValue().c_str() ) );
	else if(choices[WxChoice->GetCurrentSelection()] == _("Rotate Left") )
		thePlugin->SetImageIcon( ConvertUnicode( WxEdit->GetValue().c_str() ), 90.0 );
	else if(choices[WxChoice->GetCurrentSelection()] == _("Rotate Right") )
		thePlugin->SetImageIcon( ConvertUnicode( WxEdit->GetValue().c_str() ), 270.0 );
	else if(choices[WxChoice->GetCurrentSelection()] == _("Flip Left/Right") )
		thePlugin->SetImageIcon( ConvertUnicode( WxEdit->GetValue().c_str() ), 0.0, 1 );
	else if(choices[WxChoice->GetCurrentSelection()] == _("Flip Up/Down") )
		thePlugin->SetImageIcon( ConvertUnicode( WxEdit->GetValue().c_str() ), 0.0, 2 );
	networkFrame->Refresh();
	Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::cancelButtonClick(wxCommandEvent& event)
{
	Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::AddIconsDir(wxString directory)
{
	//Parse the default directory structure
   //wxString directory( _("F:/ASPENV21/2DIcons") );
   //wxString directory = wxString(path, wxConvUTF8);
    if( !wxDir::Exists( directory ) )
    {
        return;
    }

    //GET_ICON_STREAM( GetVESuite_Valve_Valve_VALVE4)
    
   wxString dirname;
   wxDir parentDir (directory);
   bool isParentTrue = parentDir.GetFirst(&dirname);
   int buttonCount = 2000;
   maxRows = 0;
   while(isParentTrue)
   {
      //do not include the .svn folders
      if ( dirname.compare(wxT(".svn")) != 0)
      {
         wxPanel * WxNoteBookPage = new wxPanel(WxNotebook);
         WxNotebook->AddPage(WxNoteBookPage, dirname);

         wxString filename;
         wxDir childDir (directory+ wxT("/")+dirname);
         bool isChildTrue = childDir.GetFirst(&filename);
         int hCount = 0;
         int vCount = 0;
         int xLoc = 0;
         int yLoc = 0;
         while(isChildTrue)
         {
            //do not include the .svn folders
            if ( filename.compare(wxT(".svn")) != 0 )
            {
               //construct iconPath and place it in the map along with its event id
               filename = filename.RemoveLast(4);
               wxString iconPath = dirname+ wxString(_("/"))+filename;
               iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );

               //create the image for the button and scale it
               wxInitAllImageHandlers();
               wxImage jpeg (directory + wxT("/") + iconPath + wxT(".jpg"));
               jpeg = jpeg.Scale(50, 70);

               //place the button and its label on the current page
               xLoc = 60 * hCount;
               yLoc = 80 * vCount;
               //yLoc = 95 * vCount;
               wxBitmapButton * tempButton = new wxBitmapButton(WxNoteBookPage, buttonCount, jpeg, wxPoint(xLoc, yLoc));
               tempButton->SetToolTip(filename);
               Connect(buttonCount, wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler(IconChooser::WxButtonClick));
               //wxStaticText * iconLabel = new wxStaticText(WxNoteBookPage, 9999, filename, wxPoint(xLoc, yLoc + 80), wxDefaultSize, 0, filename);

               buttonCount++;
               hCount++;

               //set how many buttons can be placed horizonatally
               //currently 10 buttons
               if ( hCount == 10 )
               {
                  hCount = 0;
                  vCount ++;
               }
            }
            isChildTrue = childDir.GetNext(&filename);
         }
		if(vCount > maxRows)
			maxRows = vCount+1;
      }
      isParentTrue = parentDir.GetNext(&dirname);
	  //button size and # of columns is fixed
	  SetSize(640, maxRows*80+125);
	  //WxPanel->SetSize(640, maxRows*80+50);
	  WxNotebook->SetSize(635, maxRows*80+25);
	  WxEdit->SetPosition(wxPoint(10, maxRows*80+30));
	  WxChoice->SetPosition(wxPoint(325, maxRows*80+30));
	  okButton->SetPosition(wxPoint(450, maxRows*80+30));
	  cancelButton->SetPosition(wxPoint(535, maxRows*80+30));
   }
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::IconDirectoryClick(wxCommandEvent& event)
{
	WxDirDialog = new wxDirDialog(this);
	WxDirDialog->ShowModal();
	AddIconsDir(WxDirDialog->GetPath());
}
////////////////////////////////////////////////////////////////////////////////
