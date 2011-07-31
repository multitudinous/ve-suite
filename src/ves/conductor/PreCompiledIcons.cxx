/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/conductor/PreCompiledIcons.h>                                                                
                                                                                                           
#include <map>                                                                                             
#include <string>                                                                                          
                                                                                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_ecn_hs_1.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_ecn_hs_2.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_ecn_ht_1.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_ecn_ht_2.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_forced_1.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_forced_2.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_induce_1.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_induce_2.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_natura_1.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_natura_2.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/analyzer_analyzer_analyze2.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/analyzer_analyzer_analyzer.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/analyzer_analyzer_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/apecs.xpm>                                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/aplusfeed_aplusfeed_arrow.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/aplusproduct_aplusproduct_arrow.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/aspen.xpm>                                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/batchfrac_batchfrac_block.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/batchfrac_batchfrac_column1.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/batchfrac_batchfrac_column2.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/batchfrac_batchfrac_column3.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/burstingdisk_burstingdisk_bdisk1.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/ccd_ccd_block.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/ccd_ccd_icon.xpm>                                                          
#include <ves/conductor/xpm/AspenPlus2DIcons/cfuge_cfuge_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/cfuge_cfuge_icon.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/clchng_clchng_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/clchng_clchng_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/comparator_comparator_icon1.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/compr_compr_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/compr_compr_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/compr_compr_icon2.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/compr_compr_icon3.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/crusher_crusher_block.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/crusher_crusher_icon.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/crystallizer_crystallizer_block.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/crystallizer_crystallizer_icon1.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/crystallizer_crystallizer_icon2.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/cyclone_cyclone_block.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/cyclone_cyclone_icon.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/dead_time_dead_time_icon1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/decanter_decanter_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/decanter_decanter_h_drum.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/decanter_decanter_v_drum.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/deltap_deltap_dp.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/discretize_discretize_icon1.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/distl_distl_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/distl_distl_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/distl_distl_icon2.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/dmcplus_dmcplus_icon1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/dmcplus_dmcplus_icon2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/dryer_dryer_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/dryer_dryer_fluidbed.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/dryer_dryer_fluidbed2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/dryer_dryer_spray.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/dstwu_dstwu_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/dstwu_dstwu_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/dstwu_dstwu_icon2.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_dupl_block.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_dupl_dot.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_dupl_heat.xpm>                                                        
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_dupl_work.xpm>                                                        
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_heat_dupl_heat_heat.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_work_dupl_work_work.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/dynamics.xpm>                                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/esp_esp_block.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/esp_esp_icon.xpm>                                                          
#include <ves/conductor/xpm/AspenPlus2DIcons/expansion_expansion_contract1.xpm>                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/expansion_expansion_expand1.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/extract_extract_block.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/extract_extract_icon1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/extract_extract_icon2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/extract_extract_pod.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/fabfl_fabfl_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/fabfl_fabfl_icon.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/feedbl_feedbl_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/feedbl_feedbl_feedbl.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/feedforward_feedforward_icon1.xpm>                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/filter_filter_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/filter_filter_plate.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/filter_filter_rotary.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_furnace.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_heater.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_horizontal.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_h_drum.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_sperical.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_vertical.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_v_drum1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_v_drum2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_furnace.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_heater.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_h_drum.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_v_drum1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_v_drum2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_3way.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_dot.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_heat.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_heat_tee.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_tee.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_triangle.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_work.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_work_tee.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_heat_fsplit_heat_heat.xpm>                                          
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_heat_fsplit_heat_heat_tee.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_heat_fsplit_heat_triangle.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_work_fsplit_work_triangle.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_work_fsplit_work_work.xpm>                                          
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_work_fsplit_work_work_tee.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_aircooler.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_compr.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_furnace.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_heater.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_pump.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_valve.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_valve2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_valve4.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_ecn_hs_1.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_ecn_hs_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_ecn_ht_1.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_ecn_ht_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_hs_1cn.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_hs_1co.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_hs_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_ht_1cn.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_ht_1co.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_ht_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_forced_1.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_forced_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_hs_2cn.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_hs_2co.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_hs_4.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_ht_2cn.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_ht_2co.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_ht_4.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_gen_hs.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_gen_ht.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_g_hs_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_g_ht_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_h_hs_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_h_ht_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_induce_1.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_induce_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j12_hs1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j12_hs2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j12_ht1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j12_ht2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j21_hs1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j21_hs2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j21_ht1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j21_ht2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_k_ht_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_natura_1.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_natura_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_simp_hs.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_simp_ht.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_x_hs_1.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_x_hs_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_x_ht_1.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_x_ht_2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_hs_1cn.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_hs_1co.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_hs_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_ht_1cn.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_ht_1co.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_ht_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_hs_2cn.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_hs_2co.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_hs_4.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_ht_2cn.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_ht_2co.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_ht_4.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_gen_hs.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_gen_ht.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_g_hs_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_g_ht_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_h_hs_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_h_ht_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j12_hs1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j12_hs2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j12_ht1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j12_ht2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j21_hs1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j21_hs2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j21_ht1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j21_ht2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_k_ht_1.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_k_ht_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_simp_hs.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_simp_ht.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_x_hs_1.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_x_hs_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_x_ht_1.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_x_ht_2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/hierarchy_hierarchy_block.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/hiloselect_hiloselect_icon1.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_hs_1cn.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_hs_1co.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_hs_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_ht_1cn.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_ht_1co.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_ht_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_hs_2cn.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_hs_2co.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_hs_4.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_ht_2cn.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_ht_2co.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_ht_4.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_gen_hs.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_gen_ht.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_g_hs_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_g_ht_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_h_hs_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_h_ht_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j12_hs1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j12_hs2.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j12_ht1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j12_ht2.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j21_hs1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j21_hs2.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j21_ht1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j21_ht2.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_k_ht_1.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_k_ht_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_simp_hs.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_simp_ht.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_x_hs_1.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_x_hs_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_x_ht_1.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_x_ht_2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/hxflux_hxflux_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/hycyc_hycyc_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/hycyc_hycyc_icon.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/iae_iae_icon1.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/ise_ise_icon1.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/kineticsest_kineticsest_icon1.xpm>                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/lag_1_lag_1_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/lead_lag_lead_lag_icon1.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/logiccompare_logiccompare_logiccompare.xpm>                                
#include <ves/conductor/xpm/AspenPlus2DIcons/logicgate_logicgate_logicgate.xpm>                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/mcompr_mcompr_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/mcompr_mcompr_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_acontlr.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_aindictr.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_block.xpm>                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_fcontlr.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_findictr.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_lcontlr.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_lindictr.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure2.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure3.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure4.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure5.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure6.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure7.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure8.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_pcontlr.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_pindictr.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_tcontlr.xpm>                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_tindictr.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_block2.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_block3.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_circ_mhx.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_cocurnt.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_counter.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_counter2.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_simp_mhx.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_heat_mixer_heat_heat.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_3way.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_dot.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_heat.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_hopper.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_screw.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_tank.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_tee.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_triangle.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_valve.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_work.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_work_mixer_work_work.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_aircol.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_block.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu1.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu3.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu4.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu5.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu6.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu7.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu8.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu9.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_petlyuk.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_pfrac.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_preflash.xpm>                                          
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_vacuum1.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_vacuum2.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/multihiloselect_multihiloselect_icon1.xpm>                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/multiply_multiply_icon1.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/multisum_multisum_icon1.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_heat_mult_heat_heat.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_mult_block.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_mult_dot.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_mult_heat.xpm>                                                        
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_mult_work.xpm>                                                        
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_work_mult_work_work.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/noise_noise_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/onoffcontrol_onoffcontrol_digitalpoint.xpm>                                
#include <ves/conductor/xpm/AspenPlus2DIcons/orifice_orifice_orifice1.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_absbr.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_block.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu1.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu10.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu10f.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu11.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu11f.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu12.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu12f.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu13.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu13f.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu14.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu14f.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu15.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu15f.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu1f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu2f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu3.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu3f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu4.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu4f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu5.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu5f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu6.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu6f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu7.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu7f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu8.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu8f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu9.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu9f.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_fcc_mf1.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_fcc_mf2.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_fract.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_pfrac.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_pfracf.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_prefl1.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_prefl1f.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_prefl2.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_prefl2f.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_strip.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_vacuum1.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_vacuum1f.xpm>                                          
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_vacuum2.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_vacuum2f.xpm>                                          
#include <ves/conductor/xpm/AspenPlus2DIcons/pidincr_pidincr_icon1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/pid_pid_icon1.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_b_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_contract.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_d_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_expand.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_h_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_u_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_v_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_b_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_contract.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_d_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_expand.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_h_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_u_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_v_pipe.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_d_pipe.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_h_pipe.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_u_pipe.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_v_pipe.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_block.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_d_pipe.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_h_pipe.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_u_pipe.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_v_pipe.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_furnace.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_heater.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_h_drum.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_v_drum1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_v_drum2.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/prbs_prbs_icon1.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/psv2_psv2_pfd1.xpm>                                                        
#include <ves/conductor/xpm/AspenPlus2DIcons/psv_psv_pfd1.xpm>                                                          
#include <ves/conductor/xpm/AspenPlus2DIcons/pump_pump_block.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/pump_pump_icon1.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/pump_pump_icon2.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/qtvec_qtvec_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/qtvec_qtvec_dot.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_absbr1.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_absbr2.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_absbr3.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_block.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_decant1.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_decant2.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_decant3.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_fract1.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_fract2.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packabs.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packcol1.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packcol2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packstr1.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packstr2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_rect.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_strip1.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_strip2.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_absbr2.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_absbr3.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_absorber.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_fract.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_packabs.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_packcol.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_packstr.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_rect.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_stripper.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_vacuum1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_vacuum2.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/ratio_ratio_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/rbatch_rbatch_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rbatch_rbatch_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_horizontal.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_sperical.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_vertical.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_furnace.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_heater.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_horizontal.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_h_drum.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_sperical.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_vertical.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_v_drum1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_v_drum2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_horizontal.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_sperical.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_vertical.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/requil_requil_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/requil_requil_icon2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/requil_requil_icon3.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs2_rgibbs2_block.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs2_rgibbs2_icon1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs2_rgibbs2_icon2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs_rgibbs_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs_rgibbs_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs_rgibbs_icon2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rplugpde_rplugpde_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/rplugpde_rplugpde_icon1.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/rplugpde_rplugpde_icon2.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/rplugpde_rplugpde_icon3.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/rplug_rplug_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/rplug_rplug_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/rplug_rplug_icon2.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/rplug_rplug_icon3.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_icon2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_icon3.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_icon4.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/ryield_ryield_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/ryield_ryield_icon2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/ryield_ryield_icon3.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/scale_scale_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_cdu1.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_cdu2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_cdu3.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_vacuum1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_vacuum2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/screen_screen_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/screen_screen_icon1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/screen_screen_icon2.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_heat_selector_heat_heat.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_block.xpm>                                               
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_dot.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_heat.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_triangle.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_work.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_work_selector_work_work.xpm>                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/sep2_sep2_block.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/sep2_sep2_icon1.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/sep2_sep2_icon2.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/sep2_sep2_icon3.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/sep_sep_block.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/sep_sep_icon1.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/sep_sep_icon2.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/sep_sep_icon3.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/signalgenerator_signalgenerator_icon1.xpm>                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/signalgenerator_signalgenerator_icon2.xpm>                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/signalselector_signalselector_icon1.xpm>                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/splitrange_splitrange_icon1.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_3way.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_ccd.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_cfuge.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_cyclone.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_dot.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_filter1.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_filter2.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_screen.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_tee.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_triangle.xpm>                                                
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_vscrub.xpm>                                                  
#include <ves/conductor/xpm/AspenPlus2DIcons/standalone_standalone_standalone.xpm>                                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/steamptot_steamptot_icon1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/steamttop_steamttop_icon1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/sum_sum_icon1.xpm>                                                         
#include <ves/conductor/xpm/AspenPlus2DIcons/swash_swash_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/swash_swash_icon.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/teemixer_teemixer_dot.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/teemixer_teemixer_tee.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/teesplitter_teesplitter_dot.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/teesplitter_teesplitter_tee.xpm>                                           
#include <ves/conductor/xpm/AspenPlus2DIcons/timedata_timedata_timedata.xpm>                                            
#include <ves/conductor/xpm/AspenPlus2DIcons/transform_transform_icon1.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/tvalve_tvalve_tval1.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_cfuge.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_cstr.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_excel.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_filter.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_fract.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_heater.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_heatx1.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_heatx2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_h_drum.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_plug.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_reactor.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_rect.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_strip.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_valve4.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_v_drum.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_cfuge.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_cstr.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_filter.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_fract.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_heater.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_heatx1.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_heatx2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_h_drum.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_plug.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_reactor.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_rect.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_strip.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_valve4.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_v_drum.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/user_user_block.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/user_user_small.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_block.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_icon1.xpm>                                                     
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_valve1.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_valve2.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_valve3.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_valve4.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/vscrub_vscrub_block.xpm>                                                   
#include <ves/conductor/xpm/AspenPlus2DIcons/vscrub_vscrub_icon.xpm>                                                    
#include <ves/conductor/xpm/AspenPlus2DIcons/sim.xpm>                                                 
#include <ves/conductor/xpm/AspenPlus2DIcons/dwsim.xpm> 
#include <ves/conductor/xpm/cad_tree_expanded.xpm>
#include <ves/conductor/xpm/cad_tree_selected.xpm>
#include <ves/conductor/xpm/cad_tree_unselected.xpm>
#include <ves/conductor/xpm/contour.xpm>
#include <ves/conductor/xpm/cspline.xpm>
#include <ves/conductor/xpm/icon1.xpm>
#include <ves/conductor/xpm/icon2.xpm>
#include <ves/conductor/xpm/icon3.xpm>
#include <ves/conductor/xpm/icon4.xpm>
#include <ves/conductor/xpm/icon5.xpm>
#include <ves/conductor/xpm/isosurface.xpm>
#include <ves/conductor/xpm/navigation32x32.xpm>
#include <ves/conductor/xpm/new_vector.xpm>
#include <ves/conductor/xpm/ROItb.xpm>
#include <ves/conductor/xpm/scalartb.xpm>
#include <ves/conductor/xpm/scalartb_bw.xpm>
#include <ves/conductor/xpm/selection32x32.xpm>                                                              
#include <ves/conductor/xpm/square.xpm>
#include <ves/conductor/xpm/streamlines.xpm>
#include <ves/conductor/xpm/vector.xpm>
#include <ves/conductor/xpm/vectortb.xpm>
#include <ves/conductor/xpm/powersim/Ps_COMPONENT.xpm>
#include <ves/conductor/xpm/powersim/Ps_PROJECT.xpm>
#include <ves/conductor/xpm/powersim/Ps_RANGES.xpm>
#include <ves/conductor/xpm/powersim/Ps_SIMULATION.xpm>
#include <ves/conductor/xpm/powersim/Ps_STUDIO.xpm>
#include <ves/conductor/xpm/powersim/Ps_UNITS.xpm>
#include <ves/conductor/xpm/powersim/Ps_VARIABLE_AUXILIARY.xpm>
#include <ves/conductor/xpm/powersim/Ps_VARIABLE_CONSTANT.xpm>
#include <ves/conductor/xpm/powersim/Ps_VARIABLE_LEVEL.xpm>
#include <ves/conductor/xpm/powersim/Ps_VARIABLE_MODEL.xpm>

std::map< std::string, char** > GetPreCompiledIconMap()
{
    std::map< std::string, char** > tempIconMap;

    tempIconMap[ "aerotran_aerotran_block"] =                             aerotran_aerotran_block;                   
    tempIconMap[ "aerotran_aerotran_ecn_hs_1"] =                          aerotran_aerotran_ecn_hs_1;                
    tempIconMap[ "aerotran_aerotran_ecn_hs_2"] =                          aerotran_aerotran_ecn_hs_2;                
    tempIconMap[ "aerotran_aerotran_ecn_ht_1"] =                          aerotran_aerotran_ecn_ht_1;                
    tempIconMap[ "aerotran_aerotran_ecn_ht_2"] =                          aerotran_aerotran_ecn_ht_2;                
    tempIconMap[ "aerotran_aerotran_forced_1"] =                          aerotran_aerotran_forced_1;                
    tempIconMap[ "aerotran_aerotran_forced_2"] =                          aerotran_aerotran_forced_2;                
    tempIconMap[ "aerotran_aerotran_induce_1"] =                          aerotran_aerotran_induce_1;                
    tempIconMap[ "aerotran_aerotran_induce_2"] =                          aerotran_aerotran_induce_2;                
    tempIconMap[ "aerotran_aerotran_natura_1"] =                          aerotran_aerotran_natura_1;                
    tempIconMap[ "aerotran_aerotran_natura_2"] =                          aerotran_aerotran_natura_2;                
    tempIconMap[ "analyzer_analyzer_analyze2"] =                          analyzer_analyzer_analyze2;                
    tempIconMap[ "analyzer_analyzer_analyzer"] =                          analyzer_analyzer_analyzer;                
    tempIconMap[ "analyzer_analyzer_block"] =                             analyzer_analyzer_block;                   
    tempIconMap[ "apecs"] =                                               apecs;                                     
    tempIconMap[ "aplusfeed_aplusfeed_arrow"] =                           aplusfeed_aplusfeed_arrow;                 
    tempIconMap[ "aplusproduct_aplusproduct_arrow"] =                     aplusproduct_aplusproduct_arrow;           
    tempIconMap[ "aspen"] =                                               aspen;                                     
    tempIconMap[ "batchfrac_batchfrac_block"] =                           batchfrac_batchfrac_block;                 
    tempIconMap[ "batchfrac_batchfrac_column1"] =                         batchfrac_batchfrac_column1;               
    tempIconMap[ "batchfrac_batchfrac_column2"] =                         batchfrac_batchfrac_column2;               
    tempIconMap[ "batchfrac_batchfrac_column3"] =                         batchfrac_batchfrac_column3;               
    tempIconMap[ "burstingdisk_burstingdisk_bdisk1"] =                    burstingdisk_burstingdisk_bdisk1;          
    tempIconMap[ "ccd_ccd_block"] =                                       ccd_ccd_block;                             
    tempIconMap[ "ccd_ccd_icon"] =                                        ccd_ccd_icon;                              
    tempIconMap[ "cfuge_cfuge_block"] =                                   cfuge_cfuge_block;                         
    tempIconMap[ "cfuge_cfuge_icon"] =                                    cfuge_cfuge_icon;                          
    tempIconMap[ "clchng_clchng_block"] =                                 clchng_clchng_block;                       
    tempIconMap[ "clchng_clchng_icon1"] =                                 clchng_clchng_icon1;                       
    tempIconMap[ "comparator_comparator_icon1"] =                         comparator_comparator_icon1;               
    tempIconMap[ "compr_compr_block"] =                                   compr_compr_block;                         
    tempIconMap[ "compr_compr_icon1"] =                                   compr_compr_icon1;                         
    tempIconMap[ "compr_compr_icon2"] =                                   compr_compr_icon2;                         
    tempIconMap[ "compr_compr_icon3"] =                                   compr_compr_icon3;                         
    tempIconMap[ "crusher_crusher_block"] =                               crusher_crusher_block;                     
    tempIconMap[ "crusher_crusher_icon"] =                                crusher_crusher_icon;                      
    tempIconMap[ "crystallizer_crystallizer_block"] =                     crystallizer_crystallizer_block;           
    tempIconMap[ "crystallizer_crystallizer_icon1"] =                     crystallizer_crystallizer_icon1;           
    tempIconMap[ "crystallizer_crystallizer_icon2"] =                     crystallizer_crystallizer_icon2;           
    tempIconMap[ "cyclone_cyclone_block"] =                               cyclone_cyclone_block;                     
    tempIconMap[ "cyclone_cyclone_icon"] =                                cyclone_cyclone_icon;                      
    tempIconMap[ "dead_time_dead_time_icon1"] =                           dead_time_dead_time_icon1;                 
    tempIconMap[ "decanter_decanter_block"] =                             decanter_decanter_block;                   
    tempIconMap[ "decanter_decanter_h_drum"] =                            decanter_decanter_h_drum;                  
    tempIconMap[ "decanter_decanter_v_drum"] =                            decanter_decanter_v_drum;                  
    tempIconMap[ "deltap_deltap_dp"] =                                    deltap_deltap_dp;                          
    tempIconMap[ "discretize_discretize_icon1"] =                         discretize_discretize_icon1;                
    tempIconMap[ "distl_distl_block"] =                                   distl_distl_block;                         
    tempIconMap[ "distl_distl_icon1"] =                                   distl_distl_icon1;                         
    tempIconMap[ "distl_distl_icon2"] =                                   distl_distl_icon2;                         
    tempIconMap[ "dmcplus_dmcplus_icon1"] =                               dmcplus_dmcplus_icon1;                     
    tempIconMap[ "dmcplus_dmcplus_icon2"] =                               dmcplus_dmcplus_icon2;                     
    tempIconMap[ "dryer_dryer_block"] =                                   dryer_dryer_block;                         
    tempIconMap[ "dryer_dryer_fluidbed"] =                                dryer_dryer_fluidbed;                      
    tempIconMap[ "dryer_dryer_fluidbed2"] =                               dryer_dryer_fluidbed2;                     
    tempIconMap[ "dryer_dryer_spray"] =                                   dryer_dryer_spray;                         
    tempIconMap[ "dstwu_dstwu_block"] =                                   dstwu_dstwu_block;                         
    tempIconMap[ "dstwu_dstwu_icon1"] =                                   dstwu_dstwu_icon1;                         
    tempIconMap[ "dstwu_dstwu_icon2"] =                                   dstwu_dstwu_icon2;                         
    tempIconMap[ "dupl_dupl_block"] =                                     dupl_dupl_block;                           
    tempIconMap[ "dupl_dupl_dot"] =                                       dupl_dupl_dot;                             
    tempIconMap[ "dupl_dupl_heat"] =                                      dupl_dupl_heat;                            
    tempIconMap[ "dupl_dupl_work"] =                                      dupl_dupl_work;                            
    tempIconMap[ "dupl_heat_dupl_heat_heat"] =                            dupl_heat_dupl_heat_heat;                  
    tempIconMap[ "dupl_work_dupl_work_work"] =                            dupl_work_dupl_work_work;                  
    tempIconMap[ "dynamics"] =                                            dynamics;                                  
    tempIconMap[ "esp_esp_block"] =                                       esp_esp_block;                             
    tempIconMap[ "esp_esp_icon"] =                                        esp_esp_icon;                              
    tempIconMap[ "expansion_expansion_contract1"] =                       expansion_expansion_contract1;             
    tempIconMap[ "expansion_expansion_expand1"] =                         expansion_expansion_expand1;               
    tempIconMap[ "extract_extract_block"] =                               extract_extract_block;                     
    tempIconMap[ "extract_extract_icon1"] =                               extract_extract_icon1;                     
    tempIconMap[ "extract_extract_icon2"] =                               extract_extract_icon2;                     
    tempIconMap[ "extract_extract_pod"] =                                 extract_extract_pod;                       
    tempIconMap[ "fabfl_fabfl_block"] =                                   fabfl_fabfl_block;                         
    tempIconMap[ "fabfl_fabfl_icon"] =                                    fabfl_fabfl_icon;                          
    tempIconMap[ "feedbl_feedbl_block"] =                                 feedbl_feedbl_block;                       
    tempIconMap[ "feedbl_feedbl_feedbl"] =                                feedbl_feedbl_feedbl;                      
    tempIconMap[ "feedforward_feedforward_icon1"] =                       feedforward_feedforward_icon1;             
    tempIconMap[ "filter_filter_block"] =                                 filter_filter_block;                       
    tempIconMap[ "filter_filter_plate"] =                                 filter_filter_plate;                       
    tempIconMap[ "filter_filter_rotary"] =                                filter_filter_rotary;                      
    tempIconMap[ "flash2_flash2_block"] =                                 flash2_flash2_block;                       
    tempIconMap[ "flash2_flash2_furnace"] =                               flash2_flash2_furnace;                     
    tempIconMap[ "flash2_flash2_heater"] =                                flash2_flash2_heater;                      
    tempIconMap[ "flash2_flash2_horizontal"] =                            flash2_flash2_horizontal;                  
    tempIconMap[ "flash2_flash2_h_drum"] =                                flash2_flash2_h_drum;                      
    tempIconMap[ "flash2_flash2_icon1"] =                                 flash2_flash2_icon1;                       
    tempIconMap[ "flash2_flash2_sperical"] =                              flash2_flash2_sperical;                    
    tempIconMap[ "flash2_flash2_vertical"] =                              flash2_flash2_vertical;                    
    tempIconMap[ "flash2_flash2_v_drum1"] =                               flash2_flash2_v_drum1;                     
    tempIconMap[ "flash2_flash2_v_drum2"] =                               flash2_flash2_v_drum2;                     
    tempIconMap[ "flash3_flash3_block"] =                                 flash3_flash3_block;                       
    tempIconMap[ "flash3_flash3_furnace"] =                               flash3_flash3_furnace;                     
    tempIconMap[ "flash3_flash3_heater"] =                                flash3_flash3_heater;                      
    tempIconMap[ "flash3_flash3_h_drum"] =                                flash3_flash3_h_drum;                      
    tempIconMap[ "flash3_flash3_v_drum1"] =                               flash3_flash3_v_drum1;                     
    tempIconMap[ "flash3_flash3_v_drum2"] =                               flash3_flash3_v_drum2;                     
    tempIconMap[ "fsplit_fsplit_3way"] =                                  fsplit_fsplit_3way;                        
    tempIconMap[ "fsplit_fsplit_block"] =                                 fsplit_fsplit_block;                       
    tempIconMap[ "fsplit_fsplit_dot"] =                                   fsplit_fsplit_dot;                         
    tempIconMap[ "fsplit_fsplit_heat"] =                                  fsplit_fsplit_heat;                        
    tempIconMap[ "fsplit_fsplit_heat_tee"] =                              fsplit_fsplit_heat_tee;                    
    tempIconMap[ "fsplit_fsplit_tee"] =                                   fsplit_fsplit_tee;                         
    tempIconMap[ "fsplit_fsplit_triangle"] =                              fsplit_fsplit_triangle;                    
    tempIconMap[ "fsplit_fsplit_work"] =                                  fsplit_fsplit_work;                        
    tempIconMap[ "fsplit_fsplit_work_tee"] =                              fsplit_fsplit_work_tee;                    
    tempIconMap[ "fsplit_heat_fsplit_heat_heat"] =                        fsplit_heat_fsplit_heat_heat;              
    tempIconMap[ "fsplit_heat_fsplit_heat_heat_tee"] =                    fsplit_heat_fsplit_heat_heat_tee;          
    tempIconMap[ "fsplit_heat_fsplit_heat_triangle"] =                    fsplit_heat_fsplit_heat_triangle;          
    tempIconMap[ "fsplit_work_fsplit_work_triangle"] =                    fsplit_work_fsplit_work_triangle;          
    tempIconMap[ "fsplit_work_fsplit_work_work"] =                        fsplit_work_fsplit_work_work;              
    tempIconMap[ "fsplit_work_fsplit_work_work_tee"] =                    fsplit_work_fsplit_work_work_tee;          
    tempIconMap[ "heater_heater_aircooler"] =                             heater_heater_aircooler;                   
    tempIconMap[ "heater_heater_block"] =                                 heater_heater_block;                       
    tempIconMap[ "heater_heater_compr"] =                                 heater_heater_compr;                       
    tempIconMap[ "heater_heater_furnace"] =                               heater_heater_furnace;                     
    tempIconMap[ "heater_heater_heater"] =                                heater_heater_heater;                      
    tempIconMap[ "heater_heater_pump"] =                                  heater_heater_pump;                        
    tempIconMap[ "heater_heater_valve"] =                                 heater_heater_valve;                       
    tempIconMap[ "heater_heater_valve2"] =                                heater_heater_valve2;                      
    tempIconMap[ "heater_heater_valve4"] =                                heater_heater_valve4;                      
    tempIconMap[ "heatx_heatx_block"] =                                   heatx_heatx_block;                         
    tempIconMap[ "heatx_heatx_ecn_hs_1"] =                                heatx_heatx_ecn_hs_1;                      
    tempIconMap[ "heatx_heatx_ecn_hs_2"] =                                heatx_heatx_ecn_hs_2;                      
    tempIconMap[ "heatx_heatx_ecn_ht_1"] =                                heatx_heatx_ecn_ht_1;                      
    tempIconMap[ "heatx_heatx_ecn_ht_2"] =                                heatx_heatx_ecn_ht_2;                      
    tempIconMap[ "heatx_heatx_e_hs_1cn"] =                                heatx_heatx_e_hs_1cn;                      
    tempIconMap[ "heatx_heatx_e_hs_1co"] =                                heatx_heatx_e_hs_1co;                      
    tempIconMap[ "heatx_heatx_e_hs_2"] =                                  heatx_heatx_e_hs_2;                        
    tempIconMap[ "heatx_heatx_e_ht_1cn"] =                                heatx_heatx_e_ht_1cn;                      
    tempIconMap[ "heatx_heatx_e_ht_1co"] =                                heatx_heatx_e_ht_1co;                      
    tempIconMap[ "heatx_heatx_e_ht_2"] =                                  heatx_heatx_e_ht_2;                        
    tempIconMap[ "heatx_heatx_forced_1"] =                                heatx_heatx_forced_1;                      
    tempIconMap[ "heatx_heatx_forced_2"] =                                heatx_heatx_forced_2;                      
    tempIconMap[ "heatx_heatx_f_hs_2cn"] =                                heatx_heatx_f_hs_2cn;                      
    tempIconMap[ "heatx_heatx_f_hs_2co"] =                                heatx_heatx_f_hs_2co;                      
    tempIconMap[ "heatx_heatx_f_hs_4"] =                                  heatx_heatx_f_hs_4;                        
    tempIconMap[ "heatx_heatx_f_ht_2cn"] =                                heatx_heatx_f_ht_2cn;                      
    tempIconMap[ "heatx_heatx_f_ht_2co"] =                                heatx_heatx_f_ht_2co;                      
    tempIconMap[ "heatx_heatx_f_ht_4"] =                                  heatx_heatx_f_ht_4;                        
    tempIconMap[ "heatx_heatx_gen_hs"] =                                  heatx_heatx_gen_hs;                        
    tempIconMap[ "heatx_heatx_gen_ht"] =                                  heatx_heatx_gen_ht;                        
    tempIconMap[ "heatx_heatx_g_hs_2"] =                                  heatx_heatx_g_hs_2;                        
    tempIconMap[ "heatx_heatx_g_ht_2"] =                                  heatx_heatx_g_ht_2;                        
    tempIconMap[ "heatx_heatx_h_hs_2"] =                                  heatx_heatx_h_hs_2;                        
    tempIconMap[ "heatx_heatx_h_ht_2"] =                                  heatx_heatx_h_ht_2;                        
    tempIconMap[ "heatx_heatx_induce_1"] =                                heatx_heatx_induce_1;                      
    tempIconMap[ "heatx_heatx_induce_2"] =                                heatx_heatx_induce_2;                      
    tempIconMap[ "heatx_heatx_j12_hs1"] =                                 heatx_heatx_j12_hs1;                       
    tempIconMap[ "heatx_heatx_j12_hs2"] =                                 heatx_heatx_j12_hs2;                       
    tempIconMap[ "heatx_heatx_j12_ht1"] =                                 heatx_heatx_j12_ht1;                       
    tempIconMap[ "heatx_heatx_j12_ht2"] =                                 heatx_heatx_j12_ht2;                       
    tempIconMap[ "heatx_heatx_j21_hs1"] =                                 heatx_heatx_j21_hs1;                       
    tempIconMap[ "heatx_heatx_j21_hs2"] =                                 heatx_heatx_j21_hs2;                       
    tempIconMap[ "heatx_heatx_j21_ht1"] =                                 heatx_heatx_j21_ht1;                       
    tempIconMap[ "heatx_heatx_j21_ht2"] =                                 heatx_heatx_j21_ht2;                       
    tempIconMap[ "heatx_heatx_k_ht_2"] =                                  heatx_heatx_k_ht_2;                        
    tempIconMap[ "heatx_heatx_natura_1"] =                                heatx_heatx_natura_1;                      
    tempIconMap[ "heatx_heatx_natura_2"] =                                heatx_heatx_natura_2;                      
    tempIconMap[ "heatx_heatx_simp_hs"] =                                 heatx_heatx_simp_hs;                       
    tempIconMap[ "heatx_heatx_simp_ht"] =                                 heatx_heatx_simp_ht;                       
    tempIconMap[ "heatx_heatx_x_hs_1"] =                                  heatx_heatx_x_hs_1;                        
    tempIconMap[ "heatx_heatx_x_hs_2"] =                                  heatx_heatx_x_hs_2;                        
    tempIconMap[ "heatx_heatx_x_ht_1"] =                                  heatx_heatx_x_ht_1;                        
    tempIconMap[ "heatx_heatx_x_ht_2"] =                                  heatx_heatx_x_ht_2;                        
    tempIconMap[ "hetran_hetran_block"] =                                 hetran_hetran_block;                       
    tempIconMap[ "hetran_hetran_e_hs_1cn"] =                              hetran_hetran_e_hs_1cn;                    
    tempIconMap[ "hetran_hetran_e_hs_1co"] =                              hetran_hetran_e_hs_1co;                    
    tempIconMap[ "hetran_hetran_e_hs_2"] =                                hetran_hetran_e_hs_2;                      
    tempIconMap[ "hetran_hetran_e_ht_1cn"] =                              hetran_hetran_e_ht_1cn;                    
    tempIconMap[ "hetran_hetran_e_ht_1co"] =                              hetran_hetran_e_ht_1co;                    
    tempIconMap[ "hetran_hetran_e_ht_2"] =                                hetran_hetran_e_ht_2;                      
    tempIconMap[ "hetran_hetran_f_hs_2cn"] =                              hetran_hetran_f_hs_2cn;                    
    tempIconMap[ "hetran_hetran_f_hs_2co"] =                              hetran_hetran_f_hs_2co;                    
    tempIconMap[ "hetran_hetran_f_hs_4"] =                                hetran_hetran_f_hs_4;                      
    tempIconMap[ "hetran_hetran_f_ht_2cn"] =                              hetran_hetran_f_ht_2cn;                    
    tempIconMap[ "hetran_hetran_f_ht_2co"] =                              hetran_hetran_f_ht_2co;                    
    tempIconMap[ "hetran_hetran_f_ht_4"] =                                hetran_hetran_f_ht_4;                      
    tempIconMap[ "hetran_hetran_gen_hs"] =                                hetran_hetran_gen_hs;                      
    tempIconMap[ "hetran_hetran_gen_ht"] =                                hetran_hetran_gen_ht;                      
    tempIconMap[ "hetran_hetran_g_hs_2"] =                                hetran_hetran_g_hs_2;                      
    tempIconMap[ "hetran_hetran_g_ht_2"] =                                hetran_hetran_g_ht_2;                      
    tempIconMap[ "hetran_hetran_h_hs_2"] =                                hetran_hetran_h_hs_2;                      
    tempIconMap[ "hetran_hetran_h_ht_2"] =                                hetran_hetran_h_ht_2;                      
    tempIconMap[ "hetran_hetran_j12_hs1"] =                               hetran_hetran_j12_hs1;                     
    tempIconMap[ "hetran_hetran_j12_hs2"] =                               hetran_hetran_j12_hs2;                     
    tempIconMap[ "hetran_hetran_j12_ht1"] =                               hetran_hetran_j12_ht1;                     
    tempIconMap[ "hetran_hetran_j12_ht2"] =                               hetran_hetran_j12_ht2;                     
    tempIconMap[ "hetran_hetran_j21_hs1"] =                               hetran_hetran_j21_hs1;                     
    tempIconMap[ "hetran_hetran_j21_hs2"] =                               hetran_hetran_j21_hs2;                     
    tempIconMap[ "hetran_hetran_j21_ht1"] =                               hetran_hetran_j21_ht1;                     
    tempIconMap[ "hetran_hetran_j21_ht2"] =                               hetran_hetran_j21_ht2;                     
    tempIconMap[ "hetran_hetran_k_ht_1"] =                                hetran_hetran_k_ht_1;                      
    tempIconMap[ "hetran_hetran_k_ht_2"] =                                hetran_hetran_k_ht_2;                      
    tempIconMap[ "hetran_hetran_simp_hs"] =                               hetran_hetran_simp_hs;                     
    tempIconMap[ "hetran_hetran_simp_ht"] =                               hetran_hetran_simp_ht;                      
    tempIconMap[ "hetran_hetran_x_hs_1"] =                                hetran_hetran_x_hs_1;                      
    tempIconMap[ "hetran_hetran_x_hs_2"] =                                hetran_hetran_x_hs_2;                      
    tempIconMap[ "hetran_hetran_x_ht_1"] =                                hetran_hetran_x_ht_1;                      
    tempIconMap[ "hetran_hetran_x_ht_2"] =                                hetran_hetran_x_ht_2;                      
    tempIconMap[ "hierarchy_hierarchy_block"] =                           hierarchy_hierarchy_block;                 
    tempIconMap[ "hiloselect_hiloselect_icon1"] =                         hiloselect_hiloselect_icon1;               
    tempIconMap[ "htrixist_htrixist_block"] =                             htrixist_htrixist_block;                   
    tempIconMap[ "htrixist_htrixist_e_hs_1cn"] =                          htrixist_htrixist_e_hs_1cn;                
    tempIconMap[ "htrixist_htrixist_e_hs_1co"] =                          htrixist_htrixist_e_hs_1co;                
    tempIconMap[ "htrixist_htrixist_e_hs_2"] =                            htrixist_htrixist_e_hs_2;                  
    tempIconMap[ "htrixist_htrixist_e_ht_1cn"] =                          htrixist_htrixist_e_ht_1cn;                
    tempIconMap[ "htrixist_htrixist_e_ht_1co"] =                          htrixist_htrixist_e_ht_1co;                
    tempIconMap[ "htrixist_htrixist_e_ht_2"] =                            htrixist_htrixist_e_ht_2;                  
    tempIconMap[ "htrixist_htrixist_f_hs_2cn"] =                          htrixist_htrixist_f_hs_2cn;                
    tempIconMap[ "htrixist_htrixist_f_hs_2co"] =                          htrixist_htrixist_f_hs_2co;                
    tempIconMap[ "htrixist_htrixist_f_hs_4"] =                            htrixist_htrixist_f_hs_4;                  
    tempIconMap[ "htrixist_htrixist_f_ht_2cn"] =                          htrixist_htrixist_f_ht_2cn;                
    tempIconMap[ "htrixist_htrixist_f_ht_2co"] =                          htrixist_htrixist_f_ht_2co;                
    tempIconMap[ "htrixist_htrixist_f_ht_4"] =                            htrixist_htrixist_f_ht_4;                  
    tempIconMap[ "htrixist_htrixist_gen_hs"] =                            htrixist_htrixist_gen_hs;                  
    tempIconMap[ "htrixist_htrixist_gen_ht"] =                            htrixist_htrixist_gen_ht;                  
    tempIconMap[ "htrixist_htrixist_g_hs_2"] =                            htrixist_htrixist_g_hs_2;                  
    tempIconMap[ "htrixist_htrixist_g_ht_2"] =                            htrixist_htrixist_g_ht_2;                  
    tempIconMap[ "htrixist_htrixist_h_hs_2"] =                            htrixist_htrixist_h_hs_2;                  
    tempIconMap[ "htrixist_htrixist_h_ht_2"] =                            htrixist_htrixist_h_ht_2;                  
    tempIconMap[ "htrixist_htrixist_j12_hs1"] =                           htrixist_htrixist_j12_hs1;                 
    tempIconMap[ "htrixist_htrixist_j12_hs2"] =                           htrixist_htrixist_j12_hs2;                 
    tempIconMap[ "htrixist_htrixist_j12_ht1"] =                           htrixist_htrixist_j12_ht1;                 
    tempIconMap[ "htrixist_htrixist_j12_ht2"] =                           htrixist_htrixist_j12_ht2;                 
    tempIconMap[ "htrixist_htrixist_j21_hs1"] =                           htrixist_htrixist_j21_hs1;                 
    tempIconMap[ "htrixist_htrixist_j21_hs2"] =                           htrixist_htrixist_j21_hs2;                 
    tempIconMap[ "htrixist_htrixist_j21_ht1"] =                           htrixist_htrixist_j21_ht1;                 
    tempIconMap[ "htrixist_htrixist_j21_ht2"] =                           htrixist_htrixist_j21_ht2;                 
    tempIconMap[ "htrixist_htrixist_k_ht_1"] =                            htrixist_htrixist_k_ht_1;                  
    tempIconMap[ "htrixist_htrixist_k_ht_2"] =                            htrixist_htrixist_k_ht_2;                  
    tempIconMap[ "htrixist_htrixist_simp_hs"] =                           htrixist_htrixist_simp_hs;                 
    tempIconMap[ "htrixist_htrixist_simp_ht"] =                           htrixist_htrixist_simp_ht;                  
    tempIconMap[ "htrixist_htrixist_x_hs_1"] =                            htrixist_htrixist_x_hs_1;                  
    tempIconMap[ "htrixist_htrixist_x_hs_2"] =                            htrixist_htrixist_x_hs_2;                  
    tempIconMap[ "htrixist_htrixist_x_ht_1"] =                            htrixist_htrixist_x_ht_1;                  
    tempIconMap[ "htrixist_htrixist_x_ht_2"] =                            htrixist_htrixist_x_ht_2;                  
    tempIconMap[ "hxflux_hxflux_block"] =                                 hxflux_hxflux_block;                       
    tempIconMap[ "hycyc_hycyc_block"] =                                   hycyc_hycyc_block;                         
    tempIconMap[ "hycyc_hycyc_icon"] =                                    hycyc_hycyc_icon;                          
    tempIconMap[ "iae_iae_icon1"] =                                       iae_iae_icon1;                             
    tempIconMap[ "ise_ise_icon1"] =                                       ise_ise_icon1;                             
    tempIconMap[ "kineticsest_kineticsest_icon1"] =                       kineticsest_kineticsest_icon1;             
    tempIconMap[ "lag_1_lag_1_icon1"] =                                   lag_1_lag_1_icon1;                         
    tempIconMap[ "lead_lag_lead_lag_icon1"] =                             lead_lag_lead_lag_icon1;                   
    tempIconMap[ "logiccompare_logiccompare_logiccompare"] =              logiccompare_logiccompare_logiccompare;    
    tempIconMap[ "logicgate_logicgate_logicgate"] =                       logicgate_logicgate_logicgate;             
    tempIconMap[ "mcompr_mcompr_block"] =                                 mcompr_mcompr_block;                       
    tempIconMap[ "mcompr_mcompr_icon1"] =                                 mcompr_mcompr_icon1;                       
    tempIconMap[ "measurement_measurement_acontlr"] =                     measurement_measurement_acontlr;           
    tempIconMap[ "measurement_measurement_aindictr"] =                    measurement_measurement_aindictr;          
    tempIconMap[ "measurement_measurement_block"] =                       measurement_measurement_block;             
    tempIconMap[ "measurement_measurement_fcontlr"] =                     measurement_measurement_fcontlr;           
    tempIconMap[ "measurement_measurement_findictr"] =                    measurement_measurement_findictr;          
    tempIconMap[ "measurement_measurement_lcontlr"] =                     measurement_measurement_lcontlr;           
    tempIconMap[ "measurement_measurement_lindictr"] =                    measurement_measurement_lindictr;          
    tempIconMap[ "measurement_measurement_measure"] =                     measurement_measurement_measure;          
    tempIconMap[ "measurement_measurement_measure2"] =                    measurement_measurement_measure2;          
    tempIconMap[ "measurement_measurement_measure3"] =                    measurement_measurement_measure3;          
    tempIconMap[ "measurement_measurement_measure4"] =                    measurement_measurement_measure4;          
    tempIconMap[ "measurement_measurement_measure5"] =                    measurement_measurement_measure5;          
    tempIconMap[ "measurement_measurement_measure6"] =                    measurement_measurement_measure6;          
    tempIconMap[ "measurement_measurement_measure7"] =                    measurement_measurement_measure7;          
    tempIconMap[ "measurement_measurement_measure8"] =                    measurement_measurement_measure8;          
    tempIconMap[ "measurement_measurement_pcontlr"] =                     measurement_measurement_pcontlr;           
    tempIconMap[ "measurement_measurement_pindictr"] =                    measurement_measurement_pindictr;          
    tempIconMap[ "measurement_measurement_tcontlr"] =                     measurement_measurement_tcontlr;           
    tempIconMap[ "measurement_measurement_tindictr"] =                    measurement_measurement_tindictr;          
    tempIconMap[ "mheatx_mheatx_block"] =                                 mheatx_mheatx_block;                       
    tempIconMap[ "mheatx_mheatx_block2"] =                                mheatx_mheatx_block2;                      
    tempIconMap[ "mheatx_mheatx_block3"] =                                mheatx_mheatx_block3;                      
    tempIconMap[ "mheatx_mheatx_circ_mhx"] =                              mheatx_mheatx_circ_mhx;                    
    tempIconMap[ "mheatx_mheatx_cocurnt"] =                               mheatx_mheatx_cocurnt;                     
    tempIconMap[ "mheatx_mheatx_counter"] =                               mheatx_mheatx_counter;                     
    tempIconMap[ "mheatx_mheatx_counter2"] =                              mheatx_mheatx_counter2;                    
    tempIconMap[ "mheatx_mheatx_icon1"] =                                 mheatx_mheatx_icon1;                       
    tempIconMap[ "mheatx_mheatx_simp_mhx"] =                              mheatx_mheatx_simp_mhx;                    
    tempIconMap[ "mixer_heat_mixer_heat_heat"] =                          mixer_heat_mixer_heat_heat;                
    tempIconMap[ "mixer_mixer_3way"] =                                    mixer_mixer_3way;                          
    tempIconMap[ "mixer_mixer_block"] =                                   mixer_mixer_block;                         
    tempIconMap[ "mixer_mixer_dot"] =                                     mixer_mixer_dot;                           
    tempIconMap[ "mixer_mixer_heat"] =                                    mixer_mixer_heat;                          
    tempIconMap[ "mixer_mixer_hopper"] =                                  mixer_mixer_hopper;                        
    tempIconMap[ "mixer_mixer_screw"] =                                   mixer_mixer_screw;                         
    tempIconMap[ "mixer_mixer_tank"] =                                    mixer_mixer_tank;                          
    tempIconMap[ "mixer_mixer_tee"] =                                     mixer_mixer_tee;                           
    tempIconMap[ "mixer_mixer_triangle"] =                                mixer_mixer_triangle;                      
    tempIconMap[ "mixer_mixer_valve"] =                                   mixer_mixer_valve;                         
    tempIconMap[ "mixer_mixer_work"] =                                    mixer_mixer_work;                          
    tempIconMap[ "mixer_work_mixer_work_work"] =                          mixer_work_mixer_work_work;                
    tempIconMap[ "multifrac_multifrac_aircol"] =                          multifrac_multifrac_aircol;                
    tempIconMap[ "multifrac_multifrac_block"] =                           multifrac_multifrac_block;                 
    tempIconMap[ "multifrac_multifrac_cdu1"] =                            multifrac_multifrac_cdu1;                  
    tempIconMap[ "multifrac_multifrac_cdu2"] =                            multifrac_multifrac_cdu2;                  
    tempIconMap[ "multifrac_multifrac_cdu3"] =                            multifrac_multifrac_cdu3;                  
    tempIconMap[ "multifrac_multifrac_cdu4"] =                            multifrac_multifrac_cdu4;                  
    tempIconMap[ "multifrac_multifrac_cdu5"] =                            multifrac_multifrac_cdu5;                  
    tempIconMap[ "multifrac_multifrac_cdu6"] =                            multifrac_multifrac_cdu6;                  
    tempIconMap[ "multifrac_multifrac_cdu7"] =                            multifrac_multifrac_cdu7;                  
    tempIconMap[ "multifrac_multifrac_cdu8"] =                            multifrac_multifrac_cdu8;                  
    tempIconMap[ "multifrac_multifrac_cdu9"] =                            multifrac_multifrac_cdu9;                  
    tempIconMap[ "multifrac_multifrac_petlyuk"] =                         multifrac_multifrac_petlyuk;               
    tempIconMap[ "multifrac_multifrac_pfrac"] =                           multifrac_multifrac_pfrac;                 
    tempIconMap[ "multifrac_multifrac_preflash"] =                        multifrac_multifrac_preflash;              
    tempIconMap[ "multifrac_multifrac_vacuum1"] =                         multifrac_multifrac_vacuum1;               
    tempIconMap[ "multifrac_multifrac_vacuum2"] =                         multifrac_multifrac_vacuum2;               
    tempIconMap[ "multihiloselect_multihiloselect_icon1"] =               multihiloselect_multihiloselect_icon1;     
    tempIconMap[ "multiply_multiply_icon1"] =                             multiply_multiply_icon1;                   
    tempIconMap[ "multisum_multisum_icon1"] =                             multisum_multisum_icon1;                   
    tempIconMap[ "mult_heat_mult_heat_heat"] =                            mult_heat_mult_heat_heat;                  
    tempIconMap[ "mult_mult_block"] =                                     mult_mult_block;                           
    tempIconMap[ "mult_mult_dot"] =                                       mult_mult_dot;                             
    tempIconMap[ "mult_mult_heat"] =                                      mult_mult_heat;                            
    tempIconMap[ "mult_mult_work"] =                                      mult_mult_work;                            
    tempIconMap[ "mult_work_mult_work_work"] =                            mult_work_mult_work_work;                  
    tempIconMap[ "noise_noise_icon1"] =                                   noise_noise_icon1;                         
    tempIconMap[ "onoffcontrol_onoffcontrol_digitalpoint"] =              onoffcontrol_onoffcontrol_digitalpoint;    
    tempIconMap[ "orifice_orifice_orifice1"] =                            orifice_orifice_orifice1;                  
    tempIconMap[ "petrofrac_petrofrac_absbr"] =                           petrofrac_petrofrac_absbr;                 
    tempIconMap[ "petrofrac_petrofrac_block"] =                           petrofrac_petrofrac_block;                 
    tempIconMap[ "petrofrac_petrofrac_cdu1"] =                            petrofrac_petrofrac_cdu1;                  
    tempIconMap[ "petrofrac_petrofrac_cdu10"] =                           petrofrac_petrofrac_cdu10;                 
    tempIconMap[ "petrofrac_petrofrac_cdu10f"] =                          petrofrac_petrofrac_cdu10f;                
    tempIconMap[ "petrofrac_petrofrac_cdu11"] =                           petrofrac_petrofrac_cdu11;                 
    tempIconMap[ "petrofrac_petrofrac_cdu11f"] =                          petrofrac_petrofrac_cdu11f;                
    tempIconMap[ "petrofrac_petrofrac_cdu12"] =                           petrofrac_petrofrac_cdu12;                 
    tempIconMap[ "petrofrac_petrofrac_cdu12f"] =                          petrofrac_petrofrac_cdu12f;                
    tempIconMap[ "petrofrac_petrofrac_cdu13"] =                           petrofrac_petrofrac_cdu13;                 
    tempIconMap[ "petrofrac_petrofrac_cdu13f"] =                          petrofrac_petrofrac_cdu13f;                
    tempIconMap[ "petrofrac_petrofrac_cdu14"] =                           petrofrac_petrofrac_cdu14;                 
    tempIconMap[ "petrofrac_petrofrac_cdu14f"] =                          petrofrac_petrofrac_cdu14f;                
    tempIconMap[ "petrofrac_petrofrac_cdu15"] =                           petrofrac_petrofrac_cdu15;                 
    tempIconMap[ "petrofrac_petrofrac_cdu15f"] =                          petrofrac_petrofrac_cdu15f;                
    tempIconMap[ "petrofrac_petrofrac_cdu1f"] =                           petrofrac_petrofrac_cdu1f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu2"] =                            petrofrac_petrofrac_cdu2;                  
    tempIconMap[ "petrofrac_petrofrac_cdu2f"] =                           petrofrac_petrofrac_cdu2f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu3"] =                            petrofrac_petrofrac_cdu3;                  
    tempIconMap[ "petrofrac_petrofrac_cdu3f"] =                           petrofrac_petrofrac_cdu3f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu4"] =                            petrofrac_petrofrac_cdu4;                  
    tempIconMap[ "petrofrac_petrofrac_cdu4f"] =                           petrofrac_petrofrac_cdu4f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu5"] =                            petrofrac_petrofrac_cdu5;                  
    tempIconMap[ "petrofrac_petrofrac_cdu5f"] =                           petrofrac_petrofrac_cdu5f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu6"] =                            petrofrac_petrofrac_cdu6;                  
    tempIconMap[ "petrofrac_petrofrac_cdu6f"] =                           petrofrac_petrofrac_cdu6f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu7"] =                            petrofrac_petrofrac_cdu7;                  
    tempIconMap[ "petrofrac_petrofrac_cdu7f"] =                           petrofrac_petrofrac_cdu7f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu8"] =                            petrofrac_petrofrac_cdu8;                  
    tempIconMap[ "petrofrac_petrofrac_cdu8f"] =                           petrofrac_petrofrac_cdu8f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu9"] =                            petrofrac_petrofrac_cdu9;                  
    tempIconMap[ "petrofrac_petrofrac_cdu9f"] =                           petrofrac_petrofrac_cdu9f;                 
    tempIconMap[ "petrofrac_petrofrac_fcc_mf1"] =                         petrofrac_petrofrac_fcc_mf1;               
    tempIconMap[ "petrofrac_petrofrac_fcc_mf2"] =                         petrofrac_petrofrac_fcc_mf2;               
    tempIconMap[ "petrofrac_petrofrac_fract"] =                           petrofrac_petrofrac_fract;                 
    tempIconMap[ "petrofrac_petrofrac_pfrac"] =                           petrofrac_petrofrac_pfrac;                 
    tempIconMap[ "petrofrac_petrofrac_pfracf"] =                          petrofrac_petrofrac_pfracf;                
    tempIconMap[ "petrofrac_petrofrac_prefl1"] =                          petrofrac_petrofrac_prefl1;                
    tempIconMap[ "petrofrac_petrofrac_prefl1f"] =                         petrofrac_petrofrac_prefl1f;               
    tempIconMap[ "petrofrac_petrofrac_prefl2"] =                          petrofrac_petrofrac_prefl2;                
    tempIconMap[ "petrofrac_petrofrac_prefl2f"] =                         petrofrac_petrofrac_prefl2f;               
    tempIconMap[ "petrofrac_petrofrac_strip"] =                           petrofrac_petrofrac_strip;                 
    tempIconMap[ "petrofrac_petrofrac_vacuum1"] =                         petrofrac_petrofrac_vacuum1;               
    tempIconMap[ "petrofrac_petrofrac_vacuum1f"] =                        petrofrac_petrofrac_vacuum1f;              
    tempIconMap[ "petrofrac_petrofrac_vacuum2"] =                         petrofrac_petrofrac_vacuum2;               
    tempIconMap[ "petrofrac_petrofrac_vacuum2f"] =                        petrofrac_petrofrac_vacuum2f;              
    tempIconMap[ "pidincr_pidincr_icon1"] =                               pidincr_pidincr_icon1;                     
    tempIconMap[ "pid_pid_icon1"] =                                       pid_pid_icon1;                             
    tempIconMap[ "pipe2_pipe2_block"] =                                   pipe2_pipe2_block;                         
    tempIconMap[ "pipe2_pipe2_b_pipe"] =                                  pipe2_pipe2_b_pipe;                        
    tempIconMap[ "pipe2_pipe2_contract"] =                                pipe2_pipe2_contract;                      
    tempIconMap[ "pipe2_pipe2_d_pipe"] =                                  pipe2_pipe2_d_pipe;                        
    tempIconMap[ "pipe2_pipe2_expand"] =                                  pipe2_pipe2_expand;                        
    tempIconMap[ "pipe2_pipe2_h_pipe"] =                                  pipe2_pipe2_h_pipe;                        
    tempIconMap[ "pipe2_pipe2_u_pipe"] =                                  pipe2_pipe2_u_pipe;                        
    tempIconMap[ "pipe2_pipe2_v_pipe"] =                                  pipe2_pipe2_v_pipe;                        
    tempIconMap[ "pipe3_pipe3_block"] =                                   pipe3_pipe3_block;                         
    tempIconMap[ "pipe3_pipe3_b_pipe"] =                                  pipe3_pipe3_b_pipe;                        
    tempIconMap[ "pipe3_pipe3_contract"] =                                pipe3_pipe3_contract;                      
    tempIconMap[ "pipe3_pipe3_d_pipe"] =                                  pipe3_pipe3_d_pipe;                        
    tempIconMap[ "pipe3_pipe3_expand"] =                                  pipe3_pipe3_expand;                        
    tempIconMap[ "pipe3_pipe3_h_pipe"] =                                  pipe3_pipe3_h_pipe;                        
    tempIconMap[ "pipe3_pipe3_u_pipe"] =                                  pipe3_pipe3_u_pipe;                        
    tempIconMap[ "pipe3_pipe3_v_pipe"] =                                  pipe3_pipe3_v_pipe;                        
    tempIconMap[ "pipeline_pipeline_block"] =                             pipeline_pipeline_block;                   
    tempIconMap[ "pipeline_pipeline_d_pipe"] =                            pipeline_pipeline_d_pipe;                  
    tempIconMap[ "pipeline_pipeline_h_pipe"] =                            pipeline_pipeline_h_pipe;                 
    tempIconMap[ "pipeline_pipeline_u_pipe"] =                            pipeline_pipeline_u_pipe;                  
    tempIconMap[ "pipeline_pipeline_v_pipe"] =                            pipeline_pipeline_v_pipe;                  
    tempIconMap[ "pipe_pipe_block"] =                                     pipe_pipe_block;                           
    tempIconMap[ "pipe_pipe_d_pipe"] =                                    pipe_pipe_d_pipe;                          
    tempIconMap[ "pipe_pipe_h_pipe"] =                                    pipe_pipe_h_pipe;                         
    tempIconMap[ "pipe_pipe_u_pipe"] =                                    pipe_pipe_u_pipe;                          
    tempIconMap[ "pipe_pipe_v_pipe"] =                                    pipe_pipe_v_pipe;                          
    tempIconMap[ "polyfrac_polyfrac_block"] =                             polyfrac_polyfrac_block;                   
    tempIconMap[ "polyfrac_polyfrac_furnace"] =                           polyfrac_polyfrac_furnace;                 
    tempIconMap[ "polyfrac_polyfrac_heater"] =                            polyfrac_polyfrac_heater;                  
    tempIconMap[ "polyfrac_polyfrac_h_drum"] =                            polyfrac_polyfrac_h_drum;                  
    tempIconMap[ "polyfrac_polyfrac_v_drum1"] =                           polyfrac_polyfrac_v_drum1;                 
    tempIconMap[ "polyfrac_polyfrac_v_drum2"] =                           polyfrac_polyfrac_v_drum2;                 
    tempIconMap[ "prbs_prbs_icon1"] =                                     prbs_prbs_icon1;                           
    tempIconMap[ "psv2_psv2_pfd1"] =                                      psv2_psv2_pfd1;                            
    tempIconMap[ "psv_psv_pfd1"] =                                        psv_psv_pfd1;                              
    tempIconMap[ "pump_pump_block"] =                                     pump_pump_block;                           
    tempIconMap[ "pump_pump_icon1"] =                                     pump_pump_icon1;                           
    tempIconMap[ "pump_pump_icon2"] =                                     pump_pump_icon2;                           
    tempIconMap[ "qtvec_qtvec_block"] =                                   qtvec_qtvec_block;                         
    tempIconMap[ "qtvec_qtvec_dot"] =                                     qtvec_qtvec_dot;                           
    tempIconMap[ "radfrac_radfrac_absbr1"] =                              radfrac_radfrac_absbr1;                    
    tempIconMap[ "radfrac_radfrac_absbr2"] =                              radfrac_radfrac_absbr2;                    
    tempIconMap[ "radfrac_radfrac_absbr3"] =                              radfrac_radfrac_absbr3;                    
    tempIconMap[ "radfrac_radfrac_block"] =                               radfrac_radfrac_block;                     
    tempIconMap[ "radfrac_radfrac_decant1"] =                             radfrac_radfrac_decant1;                   
    tempIconMap[ "radfrac_radfrac_decant2"] =                             radfrac_radfrac_decant2;                   
    tempIconMap[ "radfrac_radfrac_decant3"] =                             radfrac_radfrac_decant3;                   
    tempIconMap[ "radfrac_radfrac_fract1"] =                              radfrac_radfrac_fract1;                    
    tempIconMap[ "radfrac_radfrac_fract2"] =                              radfrac_radfrac_fract2;                    
    tempIconMap[ "radfrac_radfrac_packabs"] =                             radfrac_radfrac_packabs;                   
    tempIconMap[ "radfrac_radfrac_packcol1"] =                            radfrac_radfrac_packcol1;                  
    tempIconMap[ "radfrac_radfrac_packcol2"] =                            radfrac_radfrac_packcol2;                  
    tempIconMap[ "radfrac_radfrac_packstr1"] =                            radfrac_radfrac_packstr1;                  
    tempIconMap[ "radfrac_radfrac_packstr2"] =                            radfrac_radfrac_packstr2;                  
    tempIconMap[ "radfrac_radfrac_rect"] =                                radfrac_radfrac_rect;                      
    tempIconMap[ "radfrac_radfrac_strip1"] =                              radfrac_radfrac_strip1;                    
    tempIconMap[ "radfrac_radfrac_strip2"] =                              radfrac_radfrac_strip2;                    
    tempIconMap[ "ratefrac_ratefrac_absbr2"] =                            ratefrac_ratefrac_absbr2;                  
    tempIconMap[ "ratefrac_ratefrac_absbr3"] =                            ratefrac_ratefrac_absbr3;                  
    tempIconMap[ "ratefrac_ratefrac_absorber"] =                          ratefrac_ratefrac_absorber;                
    tempIconMap[ "ratefrac_ratefrac_block"] =                             ratefrac_ratefrac_block;                   
    tempIconMap[ "ratefrac_ratefrac_fract"] =                             ratefrac_ratefrac_fract;                   
    tempIconMap[ "ratefrac_ratefrac_packabs"] =                           ratefrac_ratefrac_packabs;                 
    tempIconMap[ "ratefrac_ratefrac_packcol"] =                           ratefrac_ratefrac_packcol;                 
    tempIconMap[ "ratefrac_ratefrac_packstr"] =                           ratefrac_ratefrac_packstr;                 
    tempIconMap[ "ratefrac_ratefrac_rect"] =                              ratefrac_ratefrac_rect;                    
    tempIconMap[ "ratefrac_ratefrac_stripper"] =                          ratefrac_ratefrac_stripper;                
    tempIconMap[ "ratefrac_ratefrac_vacuum1"] =                           ratefrac_ratefrac_vacuum1;                 
    tempIconMap[ "ratefrac_ratefrac_vacuum2"] =                           ratefrac_ratefrac_vacuum2;                 
    tempIconMap[ "ratio_ratio_icon1"] =                                   ratio_ratio_icon1;                         
    tempIconMap[ "rbatch_rbatch_block"] =                                 rbatch_rbatch_block;                       
    tempIconMap[ "rbatch_rbatch_icon1"] =                                 rbatch_rbatch_icon1;                       
    tempIconMap[ "rcstr2_rcstr2_block"] =                                 rcstr2_rcstr2_block;                       
    tempIconMap[ "rcstr2_rcstr2_horizontal"] =                            rcstr2_rcstr2_horizontal;                  
    tempIconMap[ "rcstr2_rcstr2_icon1"] =                                 rcstr2_rcstr2_icon1;                       
    tempIconMap[ "rcstr2_rcstr2_sperical"] =                              rcstr2_rcstr2_sperical;                    
    tempIconMap[ "rcstr2_rcstr2_vertical"] =                              rcstr2_rcstr2_vertical;                    
    tempIconMap[ "rcstr3_rcstr3_block"] =                                 rcstr3_rcstr3_block;                       
    tempIconMap[ "rcstr3_rcstr3_furnace"] =                               rcstr3_rcstr3_furnace;                     
    tempIconMap[ "rcstr3_rcstr3_heater"] =                                rcstr3_rcstr3_heater;                      
    tempIconMap[ "rcstr3_rcstr3_horizontal"] =                            rcstr3_rcstr3_horizontal;                  
    tempIconMap[ "rcstr3_rcstr3_h_drum"] =                                rcstr3_rcstr3_h_drum;                      
    tempIconMap[ "rcstr3_rcstr3_icon1"] =                                 rcstr3_rcstr3_icon1;                       
    tempIconMap[ "rcstr3_rcstr3_sperical"] =                              rcstr3_rcstr3_sperical;                    
    tempIconMap[ "rcstr3_rcstr3_vertical"] =                              rcstr3_rcstr3_vertical;                    
    tempIconMap[ "rcstr3_rcstr3_v_drum1"] =                               rcstr3_rcstr3_v_drum1;                     
    tempIconMap[ "rcstr3_rcstr3_v_drum2"] =                               rcstr3_rcstr3_v_drum2;                     
    tempIconMap[ "rcstr_rcstr_block"] =                                   rcstr_rcstr_block;                         
    tempIconMap[ "rcstr_rcstr_horizontal"] =                              rcstr_rcstr_horizontal;                    
    tempIconMap[ "rcstr_rcstr_icon1"] =                                   rcstr_rcstr_icon1;                         
    tempIconMap[ "rcstr_rcstr_sperical"] =                                rcstr_rcstr_sperical;                      
    tempIconMap[ "rcstr_rcstr_vertical"] =                                rcstr_rcstr_vertical;                      
    tempIconMap[ "requil_requil_block"] =                                 requil_requil_block;                       
    tempIconMap[ "requil_requil_icon2"] =                                 requil_requil_icon2;                       
    tempIconMap[ "requil_requil_icon3"] =                                 requil_requil_icon3;                       
    tempIconMap[ "rgibbs2_rgibbs2_block"] =                               rgibbs2_rgibbs2_block;                     
    tempIconMap[ "rgibbs2_rgibbs2_icon1"] =                               rgibbs2_rgibbs2_icon1;                     
    tempIconMap[ "rgibbs2_rgibbs2_icon2"] =                               rgibbs2_rgibbs2_icon2;                     
    tempIconMap[ "rgibbs_rgibbs_block"] =                                 rgibbs_rgibbs_block;                       
    tempIconMap[ "rgibbs_rgibbs_icon1"] =                                 rgibbs_rgibbs_icon1;                       
    tempIconMap[ "rgibbs_rgibbs_icon2"] =                                 rgibbs_rgibbs_icon2;                       
    tempIconMap[ "rplugpde_rplugpde_block"] =                             rplugpde_rplugpde_block;                   
    tempIconMap[ "rplugpde_rplugpde_icon1"] =                             rplugpde_rplugpde_icon1;                   
    tempIconMap[ "rplugpde_rplugpde_icon2"] =                             rplugpde_rplugpde_icon2;                   
    tempIconMap[ "rplugpde_rplugpde_icon3"] =                             rplugpde_rplugpde_icon3;                   
    tempIconMap[ "rplug_rplug_block"] =                                   rplug_rplug_block;                         
    tempIconMap[ "rplug_rplug_icon1"] =                                   rplug_rplug_icon1;                         
    tempIconMap[ "rplug_rplug_icon2"] =                                   rplug_rplug_icon2;                         
    tempIconMap[ "rplug_rplug_icon3"] =                                   rplug_rplug_icon3;                         
    tempIconMap[ "rstoic_rstoic_block"] =                                 rstoic_rstoic_block;                       
    tempIconMap[ "rstoic_rstoic_icon1"] =                                 rstoic_rstoic_icon1;                       
    tempIconMap[ "rstoic_rstoic_icon2"] =                                 rstoic_rstoic_icon2;                       
    tempIconMap[ "rstoic_rstoic_icon3"] =                                 rstoic_rstoic_icon3;                       
    tempIconMap[ "rstoic_rstoic_icon4"] =                                 rstoic_rstoic_icon4;                       
    tempIconMap[ "ryield_ryield_block"] =                                 ryield_ryield_block;                       
    tempIconMap[ "ryield_ryield_icon2"] =                                 ryield_ryield_icon2;                       
    tempIconMap[ "ryield_ryield_icon3"] =                                 ryield_ryield_icon3;                       
    tempIconMap[ "scale_scale_icon1"] =                                   scale_scale_icon1;                         
    tempIconMap[ "scfrac_scfrac_block"] =                                 scfrac_scfrac_block;                       
    tempIconMap[ "scfrac_scfrac_cdu1"] =                                  scfrac_scfrac_cdu1;                        
    tempIconMap[ "scfrac_scfrac_cdu2"] =                                  scfrac_scfrac_cdu2;                        
    tempIconMap[ "scfrac_scfrac_cdu3"] =                                  scfrac_scfrac_cdu3;                        
    tempIconMap[ "scfrac_scfrac_vacuum1"] =                               scfrac_scfrac_vacuum1;                     
    tempIconMap[ "scfrac_scfrac_vacuum2"] =                               scfrac_scfrac_vacuum2;                     
    tempIconMap[ "screen_screen_block"] =                                 screen_screen_block;                       
    tempIconMap[ "screen_screen_icon1"] =                                 screen_screen_icon1;                       
    tempIconMap[ "screen_screen_icon2"] =                                 screen_screen_icon2;                       
    tempIconMap[ "selector_heat_selector_heat_heat"] =                    selector_heat_selector_heat_heat;          
    tempIconMap[ "selector_selector_block"] =                             selector_selector_block;                   
    tempIconMap[ "selector_selector_dot"] =                               selector_selector_dot;                     
    tempIconMap[ "selector_selector_heat"] =                              selector_selector_heat;                    
    tempIconMap[ "selector_selector_triangle"] =                          selector_selector_triangle;                
    tempIconMap[ "selector_selector_work"] =                              selector_selector_work;                    
    tempIconMap[ "selector_work_selector_work_work"] =                    selector_work_selector_work_work;          
    tempIconMap[ "sep2_sep2_block"] =                                     sep2_sep2_block;                           
    tempIconMap[ "sep2_sep2_icon1"] =                                     sep2_sep2_icon1;                           
    tempIconMap[ "sep2_sep2_icon2"] =                                     sep2_sep2_icon2;                           
    tempIconMap[ "sep2_sep2_icon3"] =                                     sep2_sep2_icon3;                           
    tempIconMap[ "sep_sep_block"] =                                       sep_sep_block;                             
    tempIconMap[ "sep_sep_icon1"] =                                       sep_sep_icon1;                             
    tempIconMap[ "sep_sep_icon2"] =                                       sep_sep_icon2;                             
    tempIconMap[ "sep_sep_icon3"] =                                       sep_sep_icon3;                             
    tempIconMap[ "signalgenerator_signalgenerator_icon1"] =               signalgenerator_signalgenerator_icon1;     
    tempIconMap[ "signalgenerator_signalgenerator_icon2"] =               signalgenerator_signalgenerator_icon2;     
    tempIconMap[ "signalselector_signalselector_icon1"] =                 signalselector_signalselector_icon1;       
    tempIconMap[ "splitrange_splitrange_icon1"] =                         splitrange_splitrange_icon1;               
    tempIconMap[ "ssplit_ssplit_3way"] =                                  ssplit_ssplit_3way;                        
    tempIconMap[ "ssplit_ssplit_block"] =                                 ssplit_ssplit_block;                       
    tempIconMap[ "ssplit_ssplit_ccd"] =                                   ssplit_ssplit_ccd;                         
    tempIconMap[ "ssplit_ssplit_cfuge"] =                                 ssplit_ssplit_cfuge;                       
    tempIconMap[ "ssplit_ssplit_cyclone"] =                               ssplit_ssplit_cyclone;                     
    tempIconMap[ "ssplit_ssplit_dot"] =                                   ssplit_ssplit_dot;                         
    tempIconMap[ "ssplit_ssplit_filter1"] =                               ssplit_ssplit_filter1;                     
    tempIconMap[ "ssplit_ssplit_filter2"] =                               ssplit_ssplit_filter2;                     
    tempIconMap[ "ssplit_ssplit_screen"] =                                ssplit_ssplit_screen;                      
    tempIconMap[ "ssplit_ssplit_tee"] =                                   ssplit_ssplit_tee;                         
    tempIconMap[ "ssplit_ssplit_triangle"] =                              ssplit_ssplit_triangle;                    
    tempIconMap[ "ssplit_ssplit_vscrub"] =                                ssplit_ssplit_vscrub;                      
    tempIconMap[ "standalone_standalone_standalone"] =                    standalone_standalone_standalone;                                
    tempIconMap[ "steamptot_steamptot_icon1"] =                           steamptot_steamptot_icon1;                 
    tempIconMap[ "steamttop_steamttop_icon1"] =                           steamttop_steamttop_icon1;                 
    tempIconMap[ "sum_sum_icon1"] =                                       sum_sum_icon1;                             
    tempIconMap[ "swash_swash_block"] =                                   swash_swash_block;                         
    tempIconMap[ "swash_swash_icon"] =                                    swash_swash_icon;                          
    tempIconMap[ "teemixer_teemixer_dot"] =                               teemixer_teemixer_dot;                     
    tempIconMap[ "teemixer_teemixer_tee"] =                               teemixer_teemixer_tee;                     
    tempIconMap[ "teesplitter_teesplitter_dot"] =                         teesplitter_teesplitter_dot;               
    tempIconMap[ "teesplitter_teesplitter_tee"] =                         teesplitter_teesplitter_tee;               
    tempIconMap[ "timedata_timedata_timedata"] =                          timedata_timedata_timedata;                
    tempIconMap[ "transform_transform_icon1"] =                           transform_transform_icon1;                 
    tempIconMap[ "tvalve_tvalve_tval1"] =                                 tvalve_tvalve_tval1;                       
    tempIconMap[ "user2_user2_block"] =                                   user2_user2_block;                         
    tempIconMap[ "user2_user2_cfuge"] =                                   user2_user2_cfuge;                         
    tempIconMap[ "user2_user2_cstr"] =                                    user2_user2_cstr;                          
    tempIconMap[ "user2_user2_excel"] =                                   user2_user2_excel;                         
    tempIconMap[ "user2_user2_filter"] =                                  user2_user2_filter;                        
    tempIconMap[ "user2_user2_fract"] =                                   user2_user2_fract;                         
    tempIconMap[ "user2_user2_heater"] =                                  user2_user2_heater;                        
    tempIconMap[ "user2_user2_heatx1"] =                                  user2_user2_heatx1;                        
    tempIconMap[ "user2_user2_heatx2"] =                                  user2_user2_heatx2;                        
    tempIconMap[ "user2_user2_h_drum"] =                                  user2_user2_h_drum;                        
    tempIconMap[ "user2_user2_plug"] =                                    user2_user2_plug;                          
    tempIconMap[ "user2_user2_reactor"] =                                 user2_user2_reactor;                       
    tempIconMap[ "user2_user2_rect"] =                                    user2_user2_rect;                          
    tempIconMap[ "user2_user2_strip"] =                                   user2_user2_strip;                         
    tempIconMap[ "user2_user2_valve4"] =                                  user2_user2_valve4;                        
    tempIconMap[ "user2_user2_v_drum"] =                                  user2_user2_v_drum;                        
    tempIconMap[ "user3_user3_block"] =                                   user3_user3_block;                         
    tempIconMap[ "user3_user3_cfuge"] =                                   user3_user3_cfuge;                         
    tempIconMap[ "user3_user3_cstr"] =                                    user3_user3_cstr;                          
    tempIconMap[ "user3_user3_filter"] =                                  user3_user3_filter;                        
    tempIconMap[ "user3_user3_fract"] =                                   user3_user3_fract;                         
    tempIconMap[ "user3_user3_heater"] =                                  user3_user3_heater;                        
    tempIconMap[ "user3_user3_heatx1"] =                                  user3_user3_heatx1;                        
    tempIconMap[ "user3_user3_heatx2"] =                                  user3_user3_heatx2;                        
    tempIconMap[ "user3_user3_h_drum"] =                                  user3_user3_h_drum;                        
    tempIconMap[ "user3_user3_plug"] =                                    user3_user3_plug;                          
    tempIconMap[ "user3_user3_reactor"] =                                 user3_user3_reactor;                       
    tempIconMap[ "user3_user3_rect"] =                                    user3_user3_rect;                          
    tempIconMap[ "user3_user3_strip"] =                                   user3_user3_strip;                         
    tempIconMap[ "user3_user3_valve4"] =                                  user3_user3_valve4;                        
    tempIconMap[ "user3_user3_v_drum"] =                                  user3_user3_v_drum;                        
    tempIconMap[ "user_user_block"] =                                     user_user_block;                           
    tempIconMap[ "user_user_small"] =                                     user_user_small;                           
    tempIconMap[ "valve_valve_block"] =                                   valve_valve_block;                         
    tempIconMap[ "valve_valve_icon1"] =                                   valve_valve_icon1;                         
    tempIconMap[ "valve_valve_valve1"] =                                  valve_valve_valve1;                        
    tempIconMap[ "valve_valve_valve2"] =                                  valve_valve_valve2;                        
    tempIconMap[ "valve_valve_valve3"] =                                  valve_valve_valve3;                        
    tempIconMap[ "valve_valve_valve4"] =                                  valve_valve_valve4;                        
    tempIconMap[ "vscrub_vscrub_block"] =                                 vscrub_vscrub_block;                       
    tempIconMap[ "vscrub_vscrub_icon"] =                                  vscrub_vscrub_icon;                        
    tempIconMap[ "sim"] =                                                 sim;                  
    tempIconMap[ "dwsim"] =                                               dwsim;
    tempIconMap[ "cad_tree_expanded"] =                                   cad_tree_expanded_xpm;
    tempIconMap[ "cad_tree_selected"] =                                   cad_tree_selected_xpm;
    tempIconMap[ "cad_tree_unselected"] =                                 cad_tree_unselected_xpm;
    tempIconMap[ "contour"] =                                             contour_xpm;
    tempIconMap[ "cspline"] =                                             cspline_xpm;
    tempIconMap[ "icon1"] =                                               icon1_xpm;
    tempIconMap[ "icon2"] =                                               icon2_xpm;
    tempIconMap[ "icon3"] =                                               icon3_xpm;
    tempIconMap[ "icon4"] =                                               icon4_xpm;
    tempIconMap[ "icon5"] =                                               icon5_xpm;
    tempIconMap[ "isosurface"] =                                          isosurface_xpm;
    tempIconMap[ "navigation32x32"] =                                     navigation32x32_xpm;
    tempIconMap[ "new_vector"] =                                          new_vector_xpm;
    tempIconMap[ "ROItb"] =                                               ROItb_xpm;
    tempIconMap[ "scalartb"] =                                            scalartb_xpm;
    tempIconMap[ "scalartb_bw"] =                                         scalartb_bw_xpm;
    tempIconMap[ "selection32x32"] =                                      selection32x32_xpm;
    tempIconMap[ "square"] =                                              square_xpm;
    tempIconMap[ "DefaultPlugin"] =                                       square_xpm;
    tempIconMap[ "streamlines"] =                                         streamlines_xpm;
    tempIconMap[ "vector"] =                                              vector_xpm;
    tempIconMap[ "vectortb"] =                                            vectortb_xpm;
    tempIconMap[ "Ps_COMPONENT"] =                                        Ps_COMPONENT_xpm; 
    tempIconMap[ "Ps_PROJECT"] =                                          Ps_PROJECT_xpm; 
    tempIconMap[ "Ps_RANGES"] =                                           Ps_RANGES_xpm; 
    tempIconMap[ "Ps_SIMULATION"] =                                       Ps_SIMULATION_xpm; 
    tempIconMap[ "Ps_STUDIO"] =                                           Ps_STUDIO_xpm; 
    tempIconMap[ "Ps_UNITS"] =                                            Ps_UNITS_xpm;  
    tempIconMap[ "Ps_VARIABLE_AUXILIARY"] =                               Ps_VARIABLE_AUXILIARY_xpm;  
    tempIconMap[ "Ps_VARIABLE_CONSTANT"] =                                Ps_VARIABLE_CONSTANT_xpm;  
    tempIconMap[ "Ps_VARIABLE_LEVEL"] =                                   Ps_VARIABLE_LEVEL_xpm;  
    tempIconMap[ "Ps_VARIABLE_MODEL"] =                                   Ps_VARIABLE_MODEL_xpm;
    
    return tempIconMap;
}