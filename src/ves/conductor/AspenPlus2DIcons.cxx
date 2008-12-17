#include <ves/conductor/AspenPlus2DIcons.h>                                                                
                                                                                                           
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
//#include <ves/conductor/xpm/AspenPlus2DIcons/dicretize_discretize_icon1.xpm>                                            
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
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_smp_ht.xpm>                                                  
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
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_smp_ht.xpm>                                              
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
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure1.xpm>                                      
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
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_hi_pipe.xpm>                                             
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_u_pipe.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_v_pipe.xpm>                                              
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_block.xpm>                                                       
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_d_pipe.xpm>                                                      
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_hi_pipe.xpm>                                                     
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

std::map< std::string, char** > GetAspenPlusIconMap()
{
    std::map< std::string, char** > tempIconMap;

    tempIconMap[ "aerotran_aerotran_block.xpm"] =                             aerotran_aerotran_block;                   
    tempIconMap[ "aerotran_aerotran_ecn_hs_1.xpm"] =                          aerotran_aerotran_ecn_hs_1;                
    tempIconMap[ "aerotran_aerotran_ecn_hs_2.xpm"] =                          aerotran_aerotran_ecn_hs_2;                
    tempIconMap[ "aerotran_aerotran_ecn_ht_1.xpm"] =                          aerotran_aerotran_ecn_ht_1;                
    tempIconMap[ "aerotran_aerotran_ecn_ht_2.xpm"] =                          aerotran_aerotran_ecn_ht_2;                
    tempIconMap[ "aerotran_aerotran_forced_1.xpm"] =                          aerotran_aerotran_forced_1;                
    tempIconMap[ "aerotran_aerotran_forced_2.xpm"] =                          aerotran_aerotran_forced_2;                
    tempIconMap[ "aerotran_aerotran_induce_1.xpm"] =                          aerotran_aerotran_induce_1;                
    tempIconMap[ "aerotran_aerotran_induce_2.xpm"] =                          aerotran_aerotran_induce_2;                
    tempIconMap[ "aerotran_aerotran_natura_1.xpm"] =                          aerotran_aerotran_natura_1;                
    tempIconMap[ "aerotran_aerotran_natura_2.xpm"] =                          aerotran_aerotran_natura_2;                
    tempIconMap[ "analyzer_analyzer_analyze2.xpm"] =                          analyzer_analyzer_analyze2;                
    tempIconMap[ "analyzer_analyzer_analyzer.xpm"] =                          analyzer_analyzer_analyzer;                
    tempIconMap[ "analyzer_analyzer_block.xpm"] =                             analyzer_analyzer_block;                   
    tempIconMap[ "apecs.xpm"] =                                               apecs;                                     
    tempIconMap[ "aplusfeed_aplusfeed_arrow.xpm"] =                           aplusfeed_aplusfeed_arrow;                 
    tempIconMap[ "aplusproduct_aplusproduct_arrow.xpm"] =                     aplusproduct_aplusproduct_arrow;           
    tempIconMap[ "aspen.xpm"] =                                               aspen;                                     
    tempIconMap[ "batchfrac_batchfrac_block.xpm"] =                           batchfrac_batchfrac_block;                 
    tempIconMap[ "batchfrac_batchfrac_column1.xpm"] =                         batchfrac_batchfrac_column1;               
    tempIconMap[ "batchfrac_batchfrac_column2.xpm"] =                         batchfrac_batchfrac_column2;               
    tempIconMap[ "batchfrac_batchfrac_column3.xpm"] =                         batchfrac_batchfrac_column3;               
    tempIconMap[ "burstingdisk_burstingdisk_bdisk1.xpm"] =                    burstingdisk_burstingdisk_bdisk1;          
    tempIconMap[ "ccd_ccd_block.xpm"] =                                       ccd_ccd_block;                             
    tempIconMap[ "ccd_ccd_icon.xpm"] =                                        ccd_ccd_icon;                              
    tempIconMap[ "cfuge_cfuge_block.xpm"] =                                   cfuge_cfuge_block;                         
    tempIconMap[ "cfuge_cfuge_icon.xpm"] =                                    cfuge_cfuge_icon;                          
    tempIconMap[ "clchng_clchng_block.xpm"] =                                 clchng_clchng_block;                       
    tempIconMap[ "clchng_clchng_icon1.xpm"] =                                 clchng_clchng_icon1;                       
    tempIconMap[ "comparator_comparator_icon1.xpm"] =                         comparator_comparator_icon1;               
    tempIconMap[ "compr_compr_block.xpm"] =                                   compr_compr_block;                         
    tempIconMap[ "compr_compr_icon1.xpm"] =                                   compr_compr_icon1;                         
    tempIconMap[ "compr_compr_icon2.xpm"] =                                   compr_compr_icon2;                         
    tempIconMap[ "compr_compr_icon3.xpm"] =                                   compr_compr_icon3;                         
    tempIconMap[ "crusher_crusher_block.xpm"] =                               crusher_crusher_block;                     
    tempIconMap[ "crusher_crusher_icon.xpm"] =                                crusher_crusher_icon;                      
    tempIconMap[ "crystallizer_crystallizer_block.xpm"] =                     crystallizer_crystallizer_block;           
    tempIconMap[ "crystallizer_crystallizer_icon1.xpm"] =                     crystallizer_crystallizer_icon1;           
    tempIconMap[ "crystallizer_crystallizer_icon2.xpm"] =                     crystallizer_crystallizer_icon2;           
    tempIconMap[ "cyclone_cyclone_block.xpm"] =                               cyclone_cyclone_block;                     
    tempIconMap[ "cyclone_cyclone_icon.xpm"] =                                cyclone_cyclone_icon;                      
    tempIconMap[ "dead_time_dead_time_icon1.xpm"] =                           dead_time_dead_time_icon1;                 
    tempIconMap[ "decanter_decanter_block.xpm"] =                             decanter_decanter_block;                   
    tempIconMap[ "decanter_decanter_h_drum.xpm"] =                            decanter_decanter_h_drum;                  
    tempIconMap[ "decanter_decanter_v_drum.xpm"] =                            decanter_decanter_v_drum;                  
    tempIconMap[ "deltap_deltap_dp.xpm"] =                                    deltap_deltap_dp;                          
    //tempIconMap[ "dicretize_discretize_icon1.xpm"] =                          dicretize_discretize_icon1;                
    tempIconMap[ "distl_distl_block.xpm"] =                                   distl_distl_block;                         
    tempIconMap[ "distl_distl_icon1.xpm"] =                                   distl_distl_icon1;                         
    tempIconMap[ "distl_distl_icon2.xpm"] =                                   distl_distl_icon2;                         
    tempIconMap[ "dmcplus_dmcplus_icon1.xpm"] =                               dmcplus_dmcplus_icon1;                     
    tempIconMap[ "dmcplus_dmcplus_icon2.xpm"] =                               dmcplus_dmcplus_icon2;                     
    tempIconMap[ "dryer_dryer_block.xpm"] =                                   dryer_dryer_block;                         
    tempIconMap[ "dryer_dryer_fluidbed.xpm"] =                                dryer_dryer_fluidbed;                      
    tempIconMap[ "dryer_dryer_fluidbed2.xpm"] =                               dryer_dryer_fluidbed2;                     
    tempIconMap[ "dryer_dryer_spray.xpm"] =                                   dryer_dryer_spray;                         
    tempIconMap[ "dstwu_dstwu_block.xpm"] =                                   dstwu_dstwu_block;                         
    tempIconMap[ "dstwu_dstwu_icon1.xpm"] =                                   dstwu_dstwu_icon1;                         
    tempIconMap[ "dstwu_dstwu_icon2.xpm"] =                                   dstwu_dstwu_icon2;                         
    tempIconMap[ "dupl_dupl_block.xpm"] =                                     dupl_dupl_block;                           
    tempIconMap[ "dupl_dupl_dot.xpm"] =                                       dupl_dupl_dot;                             
    tempIconMap[ "dupl_dupl_heat.xpm"] =                                      dupl_dupl_heat;                            
    tempIconMap[ "dupl_dupl_work.xpm"] =                                      dupl_dupl_work;                            
    tempIconMap[ "dupl_heat_dupl_heat_heat.xpm"] =                            dupl_heat_dupl_heat_heat;                  
    tempIconMap[ "dupl_work_dupl_work_work.xpm"] =                            dupl_work_dupl_work_work;                  
    tempIconMap[ "dynamics.xpm"] =                                            dynamics;                                  
    tempIconMap[ "esp_esp_block.xpm"] =                                       esp_esp_block;                             
    tempIconMap[ "esp_esp_icon.xpm"] =                                        esp_esp_icon;                              
    tempIconMap[ "expansion_expansion_contract1.xpm"] =                       expansion_expansion_contract1;             
    tempIconMap[ "expansion_expansion_expand1.xpm"] =                         expansion_expansion_expand1;               
    tempIconMap[ "extract_extract_block.xpm"] =                               extract_extract_block;                     
    tempIconMap[ "extract_extract_icon1.xpm"] =                               extract_extract_icon1;                     
    tempIconMap[ "extract_extract_icon2.xpm"] =                               extract_extract_icon2;                     
    tempIconMap[ "extract_extract_pod.xpm"] =                                 extract_extract_pod;                       
    tempIconMap[ "fabfl_fabfl_block.xpm"] =                                   fabfl_fabfl_block;                         
    tempIconMap[ "fabfl_fabfl_icon.xpm"] =                                    fabfl_fabfl_icon;                          
    tempIconMap[ "feedbl_feedbl_block.xpm"] =                                 feedbl_feedbl_block;                       
    tempIconMap[ "feedbl_feedbl_feedbl.xpm"] =                                feedbl_feedbl_feedbl;                      
    tempIconMap[ "feedforward_feedforward_icon1.xpm"] =                       feedforward_feedforward_icon1;             
    tempIconMap[ "filter_filter_block.xpm"] =                                 filter_filter_block;                       
    tempIconMap[ "filter_filter_plate.xpm"] =                                 filter_filter_plate;                       
    tempIconMap[ "filter_filter_rotary.xpm"] =                                filter_filter_rotary;                      
    tempIconMap[ "flash2_flash2_block.xpm"] =                                 flash2_flash2_block;                       
    tempIconMap[ "flash2_flash2_furnace.xpm"] =                               flash2_flash2_furnace;                     
    tempIconMap[ "flash2_flash2_heater.xpm"] =                                flash2_flash2_heater;                      
    tempIconMap[ "flash2_flash2_horizontal.xpm"] =                            flash2_flash2_horizontal;                  
    tempIconMap[ "flash2_flash2_h_drum.xpm"] =                                flash2_flash2_h_drum;                      
    tempIconMap[ "flash2_flash2_icon1.xpm"] =                                 flash2_flash2_icon1;                       
    tempIconMap[ "flash2_flash2_sperical.xpm"] =                              flash2_flash2_sperical;                    
    tempIconMap[ "flash2_flash2_vertical.xpm"] =                              flash2_flash2_vertical;                    
    tempIconMap[ "flash2_flash2_v_drum1.xpm"] =                               flash2_flash2_v_drum1;                     
    tempIconMap[ "flash2_flash2_v_drum2.xpm"] =                               flash2_flash2_v_drum2;                     
    tempIconMap[ "flash3_flash3_block.xpm"] =                                 flash3_flash3_block;                       
    tempIconMap[ "flash3_flash3_furnace.xpm"] =                               flash3_flash3_furnace;                     
    tempIconMap[ "flash3_flash3_heater.xpm"] =                                flash3_flash3_heater;                      
    tempIconMap[ "flash3_flash3_h_drum.xpm"] =                                flash3_flash3_h_drum;                      
    tempIconMap[ "flash3_flash3_v_drum1.xpm"] =                               flash3_flash3_v_drum1;                     
    tempIconMap[ "flash3_flash3_v_drum2.xpm"] =                               flash3_flash3_v_drum2;                     
    tempIconMap[ "fsplit_fsplit_3way.xpm"] =                                  fsplit_fsplit_3way;                        
    tempIconMap[ "fsplit_fsplit_block.xpm"] =                                 fsplit_fsplit_block;                       
    tempIconMap[ "fsplit_fsplit_dot.xpm"] =                                   fsplit_fsplit_dot;                         
    tempIconMap[ "fsplit_fsplit_heat.xpm"] =                                  fsplit_fsplit_heat;                        
    tempIconMap[ "fsplit_fsplit_heat_tee.xpm"] =                              fsplit_fsplit_heat_tee;                    
    tempIconMap[ "fsplit_fsplit_tee.xpm"] =                                   fsplit_fsplit_tee;                         
    tempIconMap[ "fsplit_fsplit_triangle.xpm"] =                              fsplit_fsplit_triangle;                    
    tempIconMap[ "fsplit_fsplit_work.xpm"] =                                  fsplit_fsplit_work;                        
    tempIconMap[ "fsplit_fsplit_work_tee.xpm"] =                              fsplit_fsplit_work_tee;                    
    tempIconMap[ "fsplit_heat_fsplit_heat_heat.xpm"] =                        fsplit_heat_fsplit_heat_heat;              
    tempIconMap[ "fsplit_heat_fsplit_heat_heat_tee.xpm"] =                    fsplit_heat_fsplit_heat_heat_tee;          
    tempIconMap[ "fsplit_heat_fsplit_heat_triangle.xpm"] =                    fsplit_heat_fsplit_heat_triangle;          
    tempIconMap[ "fsplit_work_fsplit_work_triangle.xpm"] =                    fsplit_work_fsplit_work_triangle;          
    tempIconMap[ "fsplit_work_fsplit_work_work.xpm"] =                        fsplit_work_fsplit_work_work;              
    tempIconMap[ "fsplit_work_fsplit_work_work_tee.xpm"] =                    fsplit_work_fsplit_work_work_tee;          
    tempIconMap[ "heater_heater_aircooler.xpm"] =                             heater_heater_aircooler;                   
    tempIconMap[ "heater_heater_block.xpm"] =                                 heater_heater_block;                       
    tempIconMap[ "heater_heater_compr.xpm"] =                                 heater_heater_compr;                       
    tempIconMap[ "heater_heater_furnace.xpm"] =                               heater_heater_furnace;                     
    tempIconMap[ "heater_heater_heater.xpm"] =                                heater_heater_heater;                      
    tempIconMap[ "heater_heater_pump.xpm"] =                                  heater_heater_pump;                        
    tempIconMap[ "heater_heater_valve.xpm"] =                                 heater_heater_valve;                       
    tempIconMap[ "heater_heater_valve2.xpm"] =                                heater_heater_valve2;                      
    tempIconMap[ "heater_heater_valve4.xpm"] =                                heater_heater_valve4;                      
    tempIconMap[ "heatx_heatx_block.xpm"] =                                   heatx_heatx_block;                         
    tempIconMap[ "heatx_heatx_ecn_hs_1.xpm"] =                                heatx_heatx_ecn_hs_1;                      
    tempIconMap[ "heatx_heatx_ecn_hs_2.xpm"] =                                heatx_heatx_ecn_hs_2;                      
    tempIconMap[ "heatx_heatx_ecn_ht_1.xpm"] =                                heatx_heatx_ecn_ht_1;                      
    tempIconMap[ "heatx_heatx_ecn_ht_2.xpm"] =                                heatx_heatx_ecn_ht_2;                      
    tempIconMap[ "heatx_heatx_e_hs_1cn.xpm"] =                                heatx_heatx_e_hs_1cn;                      
    tempIconMap[ "heatx_heatx_e_hs_1co.xpm"] =                                heatx_heatx_e_hs_1co;                      
    tempIconMap[ "heatx_heatx_e_hs_2.xpm"] =                                  heatx_heatx_e_hs_2;                        
    tempIconMap[ "heatx_heatx_e_ht_1cn.xpm"] =                                heatx_heatx_e_ht_1cn;                      
    tempIconMap[ "heatx_heatx_e_ht_1co.xpm"] =                                heatx_heatx_e_ht_1co;                      
    tempIconMap[ "heatx_heatx_e_ht_2.xpm"] =                                  heatx_heatx_e_ht_2;                        
    tempIconMap[ "heatx_heatx_forced_1.xpm"] =                                heatx_heatx_forced_1;                      
    tempIconMap[ "heatx_heatx_forced_2.xpm"] =                                heatx_heatx_forced_2;                      
    tempIconMap[ "heatx_heatx_f_hs_2cn.xpm"] =                                heatx_heatx_f_hs_2cn;                      
    tempIconMap[ "heatx_heatx_f_hs_2co.xpm"] =                                heatx_heatx_f_hs_2co;                      
    tempIconMap[ "heatx_heatx_f_hs_4.xpm"] =                                  heatx_heatx_f_hs_4;                        
    tempIconMap[ "heatx_heatx_f_ht_2cn.xpm"] =                                heatx_heatx_f_ht_2cn;                      
    tempIconMap[ "heatx_heatx_f_ht_2co.xpm"] =                                heatx_heatx_f_ht_2co;                      
    tempIconMap[ "heatx_heatx_f_ht_4.xpm"] =                                  heatx_heatx_f_ht_4;                        
    tempIconMap[ "heatx_heatx_gen_hs.xpm"] =                                  heatx_heatx_gen_hs;                        
    tempIconMap[ "heatx_heatx_gen_ht.xpm"] =                                  heatx_heatx_gen_ht;                        
    tempIconMap[ "heatx_heatx_g_hs_2.xpm"] =                                  heatx_heatx_g_hs_2;                        
    tempIconMap[ "heatx_heatx_g_ht_2.xpm"] =                                  heatx_heatx_g_ht_2;                        
    tempIconMap[ "heatx_heatx_h_hs_2.xpm"] =                                  heatx_heatx_h_hs_2;                        
    tempIconMap[ "heatx_heatx_h_ht_2.xpm"] =                                  heatx_heatx_h_ht_2;                        
    tempIconMap[ "heatx_heatx_induce_1.xpm"] =                                heatx_heatx_induce_1;                      
    tempIconMap[ "heatx_heatx_induce_2.xpm"] =                                heatx_heatx_induce_2;                      
    tempIconMap[ "heatx_heatx_j12_hs1.xpm"] =                                 heatx_heatx_j12_hs1;                       
    tempIconMap[ "heatx_heatx_j12_hs2.xpm"] =                                 heatx_heatx_j12_hs2;                       
    tempIconMap[ "heatx_heatx_j12_ht1.xpm"] =                                 heatx_heatx_j12_ht1;                       
    tempIconMap[ "heatx_heatx_j12_ht2.xpm"] =                                 heatx_heatx_j12_ht2;                       
    tempIconMap[ "heatx_heatx_j21_hs1.xpm"] =                                 heatx_heatx_j21_hs1;                       
    tempIconMap[ "heatx_heatx_j21_hs2.xpm"] =                                 heatx_heatx_j21_hs2;                       
    tempIconMap[ "heatx_heatx_j21_ht1.xpm"] =                                 heatx_heatx_j21_ht1;                       
    tempIconMap[ "heatx_heatx_j21_ht2.xpm"] =                                 heatx_heatx_j21_ht2;                       
    tempIconMap[ "heatx_heatx_k_ht_2.xpm"] =                                  heatx_heatx_k_ht_2;                        
    tempIconMap[ "heatx_heatx_natura_1.xpm"] =                                heatx_heatx_natura_1;                      
    tempIconMap[ "heatx_heatx_natura_2.xpm"] =                                heatx_heatx_natura_2;                      
    tempIconMap[ "heatx_heatx_simp_hs.xpm"] =                                 heatx_heatx_simp_hs;                       
    tempIconMap[ "heatx_heatx_simp_ht.xpm"] =                                 heatx_heatx_simp_ht;                       
    tempIconMap[ "heatx_heatx_x_hs_1.xpm"] =                                  heatx_heatx_x_hs_1;                        
    tempIconMap[ "heatx_heatx_x_hs_2.xpm"] =                                  heatx_heatx_x_hs_2;                        
    tempIconMap[ "heatx_heatx_x_ht_1.xpm"] =                                  heatx_heatx_x_ht_1;                        
    tempIconMap[ "heatx_heatx_x_ht_2.xpm"] =                                  heatx_heatx_x_ht_2;                        
    tempIconMap[ "hetran_hetran_block.xpm"] =                                 hetran_hetran_block;                       
    tempIconMap[ "hetran_hetran_e_hs_1cn.xpm"] =                              hetran_hetran_e_hs_1cn;                    
    tempIconMap[ "hetran_hetran_e_hs_1co.xpm"] =                              hetran_hetran_e_hs_1co;                    
    tempIconMap[ "hetran_hetran_e_hs_2.xpm"] =                                hetran_hetran_e_hs_2;                      
    tempIconMap[ "hetran_hetran_e_ht_1cn.xpm"] =                              hetran_hetran_e_ht_1cn;                    
    tempIconMap[ "hetran_hetran_e_ht_1co.xpm"] =                              hetran_hetran_e_ht_1co;                    
    tempIconMap[ "hetran_hetran_e_ht_2.xpm"] =                                hetran_hetran_e_ht_2;                      
    tempIconMap[ "hetran_hetran_f_hs_2cn.xpm"] =                              hetran_hetran_f_hs_2cn;                    
    tempIconMap[ "hetran_hetran_f_hs_2co.xpm"] =                              hetran_hetran_f_hs_2co;                    
    tempIconMap[ "hetran_hetran_f_hs_4.xpm"] =                                hetran_hetran_f_hs_4;                      
    tempIconMap[ "hetran_hetran_f_ht_2cn.xpm"] =                              hetran_hetran_f_ht_2cn;                    
    tempIconMap[ "hetran_hetran_f_ht_2co.xpm"] =                              hetran_hetran_f_ht_2co;                    
    tempIconMap[ "hetran_hetran_f_ht_4.xpm"] =                                hetran_hetran_f_ht_4;                      
    tempIconMap[ "hetran_hetran_gen_hs.xpm"] =                                hetran_hetran_gen_hs;                      
    tempIconMap[ "hetran_hetran_gen_ht.xpm"] =                                hetran_hetran_gen_ht;                      
    tempIconMap[ "hetran_hetran_g_hs_2.xpm"] =                                hetran_hetran_g_hs_2;                      
    tempIconMap[ "hetran_hetran_g_ht_2.xpm"] =                                hetran_hetran_g_ht_2;                      
    tempIconMap[ "hetran_hetran_h_hs_2.xpm"] =                                hetran_hetran_h_hs_2;                      
    tempIconMap[ "hetran_hetran_h_ht_2.xpm"] =                                hetran_hetran_h_ht_2;                      
    tempIconMap[ "hetran_hetran_j12_hs1.xpm"] =                               hetran_hetran_j12_hs1;                     
    tempIconMap[ "hetran_hetran_j12_hs2.xpm"] =                               hetran_hetran_j12_hs2;                     
    tempIconMap[ "hetran_hetran_j12_ht1.xpm"] =                               hetran_hetran_j12_ht1;                     
    tempIconMap[ "hetran_hetran_j12_ht2.xpm"] =                               hetran_hetran_j12_ht2;                     
    tempIconMap[ "hetran_hetran_j21_hs1.xpm"] =                               hetran_hetran_j21_hs1;                     
    tempIconMap[ "hetran_hetran_j21_hs2.xpm"] =                               hetran_hetran_j21_hs2;                     
    tempIconMap[ "hetran_hetran_j21_ht1.xpm"] =                               hetran_hetran_j21_ht1;                     
    tempIconMap[ "hetran_hetran_j21_ht2.xpm"] =                               hetran_hetran_j21_ht2;                     
    tempIconMap[ "hetran_hetran_k_ht_1.xpm"] =                                hetran_hetran_k_ht_1;                      
    tempIconMap[ "hetran_hetran_k_ht_2.xpm"] =                                hetran_hetran_k_ht_2;                      
    tempIconMap[ "hetran_hetran_simp_hs.xpm"] =                               hetran_hetran_simp_hs;                     
    tempIconMap[ "hetran_hetran_smp_ht.xpm"] =                                hetran_hetran_smp_ht;                      
    tempIconMap[ "hetran_hetran_x_hs_1.xpm"] =                                hetran_hetran_x_hs_1;                      
    tempIconMap[ "hetran_hetran_x_hs_2.xpm"] =                                hetran_hetran_x_hs_2;                      
    tempIconMap[ "hetran_hetran_x_ht_1.xpm"] =                                hetran_hetran_x_ht_1;                      
    tempIconMap[ "hetran_hetran_x_ht_2.xpm"] =                                hetran_hetran_x_ht_2;                      
    tempIconMap[ "hierarchy_hierarchy_block.xpm"] =                           hierarchy_hierarchy_block;                 
    tempIconMap[ "hiloselect_hiloselect_icon1.xpm"] =                         hiloselect_hiloselect_icon1;               
    tempIconMap[ "htrixist_htrixist_block.xpm"] =                             htrixist_htrixist_block;                   
    tempIconMap[ "htrixist_htrixist_e_hs_1cn.xpm"] =                          htrixist_htrixist_e_hs_1cn;                
    tempIconMap[ "htrixist_htrixist_e_hs_1co.xpm"] =                          htrixist_htrixist_e_hs_1co;                
    tempIconMap[ "htrixist_htrixist_e_hs_2.xpm"] =                            htrixist_htrixist_e_hs_2;                  
    tempIconMap[ "htrixist_htrixist_e_ht_1cn.xpm"] =                          htrixist_htrixist_e_ht_1cn;                
    tempIconMap[ "htrixist_htrixist_e_ht_1co.xpm"] =                          htrixist_htrixist_e_ht_1co;                
    tempIconMap[ "htrixist_htrixist_e_ht_2.xpm"] =                            htrixist_htrixist_e_ht_2;                  
    tempIconMap[ "htrixist_htrixist_f_hs_2cn.xpm"] =                          htrixist_htrixist_f_hs_2cn;                
    tempIconMap[ "htrixist_htrixist_f_hs_2co.xpm"] =                          htrixist_htrixist_f_hs_2co;                
    tempIconMap[ "htrixist_htrixist_f_hs_4.xpm"] =                            htrixist_htrixist_f_hs_4;                  
    tempIconMap[ "htrixist_htrixist_f_ht_2cn.xpm"] =                          htrixist_htrixist_f_ht_2cn;                
    tempIconMap[ "htrixist_htrixist_f_ht_2co.xpm"] =                          htrixist_htrixist_f_ht_2co;                
    tempIconMap[ "htrixist_htrixist_f_ht_4.xpm"] =                            htrixist_htrixist_f_ht_4;                  
    tempIconMap[ "htrixist_htrixist_gen_hs.xpm"] =                            htrixist_htrixist_gen_hs;                  
    tempIconMap[ "htrixist_htrixist_gen_ht.xpm"] =                            htrixist_htrixist_gen_ht;                  
    tempIconMap[ "htrixist_htrixist_g_hs_2.xpm"] =                            htrixist_htrixist_g_hs_2;                  
    tempIconMap[ "htrixist_htrixist_g_ht_2.xpm"] =                            htrixist_htrixist_g_ht_2;                  
    tempIconMap[ "htrixist_htrixist_h_hs_2.xpm"] =                            htrixist_htrixist_h_hs_2;                  
    tempIconMap[ "htrixist_htrixist_h_ht_2.xpm"] =                            htrixist_htrixist_h_ht_2;                  
    tempIconMap[ "htrixist_htrixist_j12_hs1.xpm"] =                           htrixist_htrixist_j12_hs1;                 
    tempIconMap[ "htrixist_htrixist_j12_hs2.xpm"] =                           htrixist_htrixist_j12_hs2;                 
    tempIconMap[ "htrixist_htrixist_j12_ht1.xpm"] =                           htrixist_htrixist_j12_ht1;                 
    tempIconMap[ "htrixist_htrixist_j12_ht2.xpm"] =                           htrixist_htrixist_j12_ht2;                 
    tempIconMap[ "htrixist_htrixist_j21_hs1.xpm"] =                           htrixist_htrixist_j21_hs1;                 
    tempIconMap[ "htrixist_htrixist_j21_hs2.xpm"] =                           htrixist_htrixist_j21_hs2;                 
    tempIconMap[ "htrixist_htrixist_j21_ht1.xpm"] =                           htrixist_htrixist_j21_ht1;                 
    tempIconMap[ "htrixist_htrixist_j21_ht2.xpm"] =                           htrixist_htrixist_j21_ht2;                 
    tempIconMap[ "htrixist_htrixist_k_ht_1.xpm"] =                            htrixist_htrixist_k_ht_1;                  
    tempIconMap[ "htrixist_htrixist_k_ht_2.xpm"] =                            htrixist_htrixist_k_ht_2;                  
    tempIconMap[ "htrixist_htrixist_simp_hs.xpm"] =                           htrixist_htrixist_simp_hs;                 
    tempIconMap[ "htrixist_htrixist_smp_ht.xpm"] =                            htrixist_htrixist_smp_ht;                  
    tempIconMap[ "htrixist_htrixist_x_hs_1.xpm"] =                            htrixist_htrixist_x_hs_1;                  
    tempIconMap[ "htrixist_htrixist_x_hs_2.xpm"] =                            htrixist_htrixist_x_hs_2;                  
    tempIconMap[ "htrixist_htrixist_x_ht_1.xpm"] =                            htrixist_htrixist_x_ht_1;                  
    tempIconMap[ "htrixist_htrixist_x_ht_2.xpm"] =                            htrixist_htrixist_x_ht_2;                  
    tempIconMap[ "hxflux_hxflux_block.xpm"] =                                 hxflux_hxflux_block;                       
    tempIconMap[ "hycyc_hycyc_block.xpm"] =                                   hycyc_hycyc_block;                         
    tempIconMap[ "hycyc_hycyc_icon.xpm"] =                                    hycyc_hycyc_icon;                          
    tempIconMap[ "iae_iae_icon1.xpm"] =                                       iae_iae_icon1;                             
    tempIconMap[ "ise_ise_icon1.xpm"] =                                       ise_ise_icon1;                             
    tempIconMap[ "kineticsest_kineticsest_icon1.xpm"] =                       kineticsest_kineticsest_icon1;             
    tempIconMap[ "lag_1_lag_1_icon1.xpm"] =                                   lag_1_lag_1_icon1;                         
    tempIconMap[ "lead_lag_lead_lag_icon1.xpm"] =                             lead_lag_lead_lag_icon1;                   
    tempIconMap[ "logiccompare_logiccompare_logiccompare.xpm"] =              logiccompare_logiccompare_logiccompare;    
    tempIconMap[ "logicgate_logicgate_logicgate.xpm"] =                       logicgate_logicgate_logicgate;             
    tempIconMap[ "mcompr_mcompr_block.xpm"] =                                 mcompr_mcompr_block;                       
    tempIconMap[ "mcompr_mcompr_icon1.xpm"] =                                 mcompr_mcompr_icon1;                       
    tempIconMap[ "measurement_measurement_acontlr.xpm"] =                     measurement_measurement_acontlr;           
    tempIconMap[ "measurement_measurement_aindictr.xpm"] =                    measurement_measurement_aindictr;          
    tempIconMap[ "measurement_measurement_block.xpm"] =                       measurement_measurement_block;             
    tempIconMap[ "measurement_measurement_fcontlr.xpm"] =                     measurement_measurement_fcontlr;           
    tempIconMap[ "measurement_measurement_findictr.xpm"] =                    measurement_measurement_findictr;          
    tempIconMap[ "measurement_measurement_lcontlr.xpm"] =                     measurement_measurement_lcontlr;           
    tempIconMap[ "measurement_measurement_lindictr.xpm"] =                    measurement_measurement_lindictr;          
    tempIconMap[ "measurement_measurement_measure1.xpm"] =                    measurement_measurement_measure1;          
    tempIconMap[ "measurement_measurement_measure2.xpm"] =                    measurement_measurement_measure2;          
    tempIconMap[ "measurement_measurement_measure3.xpm"] =                    measurement_measurement_measure3;          
    tempIconMap[ "measurement_measurement_measure4.xpm"] =                    measurement_measurement_measure4;          
    tempIconMap[ "measurement_measurement_measure5.xpm"] =                    measurement_measurement_measure5;          
    tempIconMap[ "measurement_measurement_measure6.xpm"] =                    measurement_measurement_measure6;          
    tempIconMap[ "measurement_measurement_measure7.xpm"] =                    measurement_measurement_measure7;          
    tempIconMap[ "measurement_measurement_measure8.xpm"] =                    measurement_measurement_measure8;          
    tempIconMap[ "measurement_measurement_pcontlr.xpm"] =                     measurement_measurement_pcontlr;           
    tempIconMap[ "measurement_measurement_pindictr.xpm"] =                    measurement_measurement_pindictr;          
    tempIconMap[ "measurement_measurement_tcontlr.xpm"] =                     measurement_measurement_tcontlr;           
    tempIconMap[ "measurement_measurement_tindictr.xpm"] =                    measurement_measurement_tindictr;          
    tempIconMap[ "mheatx_mheatx_block.xpm"] =                                 mheatx_mheatx_block;                       
    tempIconMap[ "mheatx_mheatx_block2.xpm"] =                                mheatx_mheatx_block2;                      
    tempIconMap[ "mheatx_mheatx_block3.xpm"] =                                mheatx_mheatx_block3;                      
    tempIconMap[ "mheatx_mheatx_circ_mhx.xpm"] =                              mheatx_mheatx_circ_mhx;                    
    tempIconMap[ "mheatx_mheatx_cocurnt.xpm"] =                               mheatx_mheatx_cocurnt;                     
    tempIconMap[ "mheatx_mheatx_counter.xpm"] =                               mheatx_mheatx_counter;                     
    tempIconMap[ "mheatx_mheatx_counter2.xpm"] =                              mheatx_mheatx_counter2;                    
    tempIconMap[ "mheatx_mheatx_icon1.xpm"] =                                 mheatx_mheatx_icon1;                       
    tempIconMap[ "mheatx_mheatx_simp_mhx.xpm"] =                              mheatx_mheatx_simp_mhx;                    
    tempIconMap[ "mixer_heat_mixer_heat_heat.xpm"] =                          mixer_heat_mixer_heat_heat;                
    tempIconMap[ "mixer_mixer_3way.xpm"] =                                    mixer_mixer_3way;                          
    tempIconMap[ "mixer_mixer_block.xpm"] =                                   mixer_mixer_block;                         
    tempIconMap[ "mixer_mixer_dot.xpm"] =                                     mixer_mixer_dot;                           
    tempIconMap[ "mixer_mixer_heat.xpm"] =                                    mixer_mixer_heat;                          
    tempIconMap[ "mixer_mixer_hopper.xpm"] =                                  mixer_mixer_hopper;                        
    tempIconMap[ "mixer_mixer_screw.xpm"] =                                   mixer_mixer_screw;                         
    tempIconMap[ "mixer_mixer_tank.xpm"] =                                    mixer_mixer_tank;                          
    tempIconMap[ "mixer_mixer_tee.xpm"] =                                     mixer_mixer_tee;                           
    tempIconMap[ "mixer_mixer_triangle.xpm"] =                                mixer_mixer_triangle;                      
    tempIconMap[ "mixer_mixer_valve.xpm"] =                                   mixer_mixer_valve;                         
    tempIconMap[ "mixer_mixer_work.xpm"] =                                    mixer_mixer_work;                          
    tempIconMap[ "mixer_work_mixer_work_work.xpm"] =                          mixer_work_mixer_work_work;                
    tempIconMap[ "multifrac_multifrac_aircol.xpm"] =                          multifrac_multifrac_aircol;                
    tempIconMap[ "multifrac_multifrac_block.xpm"] =                           multifrac_multifrac_block;                 
    tempIconMap[ "multifrac_multifrac_cdu1.xpm"] =                            multifrac_multifrac_cdu1;                  
    tempIconMap[ "multifrac_multifrac_cdu2.xpm"] =                            multifrac_multifrac_cdu2;                  
    tempIconMap[ "multifrac_multifrac_cdu3.xpm"] =                            multifrac_multifrac_cdu3;                  
    tempIconMap[ "multifrac_multifrac_cdu4.xpm"] =                            multifrac_multifrac_cdu4;                  
    tempIconMap[ "multifrac_multifrac_cdu5.xpm"] =                            multifrac_multifrac_cdu5;                  
    tempIconMap[ "multifrac_multifrac_cdu6.xpm"] =                            multifrac_multifrac_cdu6;                  
    tempIconMap[ "multifrac_multifrac_cdu7.xpm"] =                            multifrac_multifrac_cdu7;                  
    tempIconMap[ "multifrac_multifrac_cdu8.xpm"] =                            multifrac_multifrac_cdu8;                  
    tempIconMap[ "multifrac_multifrac_cdu9.xpm"] =                            multifrac_multifrac_cdu9;                  
    tempIconMap[ "multifrac_multifrac_petlyuk.xpm"] =                         multifrac_multifrac_petlyuk;               
    tempIconMap[ "multifrac_multifrac_pfrac.xpm"] =                           multifrac_multifrac_pfrac;                 
    tempIconMap[ "multifrac_multifrac_preflash.xpm"] =                        multifrac_multifrac_preflash;              
    tempIconMap[ "multifrac_multifrac_vacuum1.xpm"] =                         multifrac_multifrac_vacuum1;               
    tempIconMap[ "multifrac_multifrac_vacuum2.xpm"] =                         multifrac_multifrac_vacuum2;               
    tempIconMap[ "multihiloselect_multihiloselect_icon1.xpm"] =               multihiloselect_multihiloselect_icon1;     
    tempIconMap[ "multiply_multiply_icon1.xpm"] =                             multiply_multiply_icon1;                   
    tempIconMap[ "multisum_multisum_icon1.xpm"] =                             multisum_multisum_icon1;                   
    tempIconMap[ "mult_heat_mult_heat_heat.xpm"] =                            mult_heat_mult_heat_heat;                  
    tempIconMap[ "mult_mult_block.xpm"] =                                     mult_mult_block;                           
    tempIconMap[ "mult_mult_dot.xpm"] =                                       mult_mult_dot;                             
    tempIconMap[ "mult_mult_heat.xpm"] =                                      mult_mult_heat;                            
    tempIconMap[ "mult_mult_work.xpm"] =                                      mult_mult_work;                            
    tempIconMap[ "mult_work_mult_work_work.xpm"] =                            mult_work_mult_work_work;                  
    tempIconMap[ "noise_noise_icon1.xpm"] =                                   noise_noise_icon1;                         
    tempIconMap[ "onoffcontrol_onoffcontrol_digitalpoint.xpm"] =              onoffcontrol_onoffcontrol_digitalpoint;    
    tempIconMap[ "orifice_orifice_orifice1.xpm"] =                            orifice_orifice_orifice1;                  
    tempIconMap[ "petrofrac_petrofrac_absbr.xpm"] =                           petrofrac_petrofrac_absbr;                 
    tempIconMap[ "petrofrac_petrofrac_block.xpm"] =                           petrofrac_petrofrac_block;                 
    tempIconMap[ "petrofrac_petrofrac_cdu1.xpm"] =                            petrofrac_petrofrac_cdu1;                  
    tempIconMap[ "petrofrac_petrofrac_cdu10.xpm"] =                           petrofrac_petrofrac_cdu10;                 
    tempIconMap[ "petrofrac_petrofrac_cdu10f.xpm"] =                          petrofrac_petrofrac_cdu10f;                
    tempIconMap[ "petrofrac_petrofrac_cdu11.xpm"] =                           petrofrac_petrofrac_cdu11;                 
    tempIconMap[ "petrofrac_petrofrac_cdu11f.xpm"] =                          petrofrac_petrofrac_cdu11f;                
    tempIconMap[ "petrofrac_petrofrac_cdu12.xpm"] =                           petrofrac_petrofrac_cdu12;                 
    tempIconMap[ "petrofrac_petrofrac_cdu12f.xpm"] =                          petrofrac_petrofrac_cdu12f;                
    tempIconMap[ "petrofrac_petrofrac_cdu13.xpm"] =                           petrofrac_petrofrac_cdu13;                 
    tempIconMap[ "petrofrac_petrofrac_cdu13f.xpm"] =                          petrofrac_petrofrac_cdu13f;                
    tempIconMap[ "petrofrac_petrofrac_cdu14.xpm"] =                           petrofrac_petrofrac_cdu14;                 
    tempIconMap[ "petrofrac_petrofrac_cdu14f.xpm"] =                          petrofrac_petrofrac_cdu14f;                
    tempIconMap[ "petrofrac_petrofrac_cdu15.xpm"] =                           petrofrac_petrofrac_cdu15;                 
    tempIconMap[ "petrofrac_petrofrac_cdu15f.xpm"] =                          petrofrac_petrofrac_cdu15f;                
    tempIconMap[ "petrofrac_petrofrac_cdu1f.xpm"] =                           petrofrac_petrofrac_cdu1f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu2.xpm"] =                            petrofrac_petrofrac_cdu2;                  
    tempIconMap[ "petrofrac_petrofrac_cdu2f.xpm"] =                           petrofrac_petrofrac_cdu2f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu3.xpm"] =                            petrofrac_petrofrac_cdu3;                  
    tempIconMap[ "petrofrac_petrofrac_cdu3f.xpm"] =                           petrofrac_petrofrac_cdu3f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu4.xpm"] =                            petrofrac_petrofrac_cdu4;                  
    tempIconMap[ "petrofrac_petrofrac_cdu4f.xpm"] =                           petrofrac_petrofrac_cdu4f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu5.xpm"] =                            petrofrac_petrofrac_cdu5;                  
    tempIconMap[ "petrofrac_petrofrac_cdu5f.xpm"] =                           petrofrac_petrofrac_cdu5f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu6.xpm"] =                            petrofrac_petrofrac_cdu6;                  
    tempIconMap[ "petrofrac_petrofrac_cdu6f.xpm"] =                           petrofrac_petrofrac_cdu6f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu7.xpm"] =                            petrofrac_petrofrac_cdu7;                  
    tempIconMap[ "petrofrac_petrofrac_cdu7f.xpm"] =                           petrofrac_petrofrac_cdu7f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu8.xpm"] =                            petrofrac_petrofrac_cdu8;                  
    tempIconMap[ "petrofrac_petrofrac_cdu8f.xpm"] =                           petrofrac_petrofrac_cdu8f;                 
    tempIconMap[ "petrofrac_petrofrac_cdu9.xpm"] =                            petrofrac_petrofrac_cdu9;                  
    tempIconMap[ "petrofrac_petrofrac_cdu9f.xpm"] =                           petrofrac_petrofrac_cdu9f;                 
    tempIconMap[ "petrofrac_petrofrac_fcc_mf1.xpm"] =                         petrofrac_petrofrac_fcc_mf1;               
    tempIconMap[ "petrofrac_petrofrac_fcc_mf2.xpm"] =                         petrofrac_petrofrac_fcc_mf2;               
    tempIconMap[ "petrofrac_petrofrac_fract.xpm"] =                           petrofrac_petrofrac_fract;                 
    tempIconMap[ "petrofrac_petrofrac_pfrac.xpm"] =                           petrofrac_petrofrac_pfrac;                 
    tempIconMap[ "petrofrac_petrofrac_pfracf.xpm"] =                          petrofrac_petrofrac_pfracf;                
    tempIconMap[ "petrofrac_petrofrac_prefl1.xpm"] =                          petrofrac_petrofrac_prefl1;                
    tempIconMap[ "petrofrac_petrofrac_prefl1f.xpm"] =                         petrofrac_petrofrac_prefl1f;               
    tempIconMap[ "petrofrac_petrofrac_prefl2.xpm"] =                          petrofrac_petrofrac_prefl2;                
    tempIconMap[ "petrofrac_petrofrac_prefl2f.xpm"] =                         petrofrac_petrofrac_prefl2f;               
    tempIconMap[ "petrofrac_petrofrac_strip.xpm"] =                           petrofrac_petrofrac_strip;                 
    tempIconMap[ "petrofrac_petrofrac_vacuum1.xpm"] =                         petrofrac_petrofrac_vacuum1;               
    tempIconMap[ "petrofrac_petrofrac_vacuum1f.xpm"] =                        petrofrac_petrofrac_vacuum1f;              
    tempIconMap[ "petrofrac_petrofrac_vacuum2.xpm"] =                         petrofrac_petrofrac_vacuum2;               
    tempIconMap[ "petrofrac_petrofrac_vacuum2f.xpm"] =                        petrofrac_petrofrac_vacuum2f;              
    tempIconMap[ "pidincr_pidincr_icon1.xpm"] =                               pidincr_pidincr_icon1;                     
    tempIconMap[ "pid_pid_icon1.xpm"] =                                       pid_pid_icon1;                             
    tempIconMap[ "pipe2_pipe2_block.xpm"] =                                   pipe2_pipe2_block;                         
    tempIconMap[ "pipe2_pipe2_b_pipe.xpm"] =                                  pipe2_pipe2_b_pipe;                        
    tempIconMap[ "pipe2_pipe2_contract.xpm"] =                                pipe2_pipe2_contract;                      
    tempIconMap[ "pipe2_pipe2_d_pipe.xpm"] =                                  pipe2_pipe2_d_pipe;                        
    tempIconMap[ "pipe2_pipe2_expand.xpm"] =                                  pipe2_pipe2_expand;                        
    tempIconMap[ "pipe2_pipe2_h_pipe.xpm"] =                                  pipe2_pipe2_h_pipe;                        
    tempIconMap[ "pipe2_pipe2_u_pipe.xpm"] =                                  pipe2_pipe2_u_pipe;                        
    tempIconMap[ "pipe2_pipe2_v_pipe.xpm"] =                                  pipe2_pipe2_v_pipe;                        
    tempIconMap[ "pipe3_pipe3_block.xpm"] =                                   pipe3_pipe3_block;                         
    tempIconMap[ "pipe3_pipe3_b_pipe.xpm"] =                                  pipe3_pipe3_b_pipe;                        
    tempIconMap[ "pipe3_pipe3_contract.xpm"] =                                pipe3_pipe3_contract;                      
    tempIconMap[ "pipe3_pipe3_d_pipe.xpm"] =                                  pipe3_pipe3_d_pipe;                        
    tempIconMap[ "pipe3_pipe3_expand.xpm"] =                                  pipe3_pipe3_expand;                        
    tempIconMap[ "pipe3_pipe3_h_pipe.xpm"] =                                  pipe3_pipe3_h_pipe;                        
    tempIconMap[ "pipe3_pipe3_u_pipe.xpm"] =                                  pipe3_pipe3_u_pipe;                        
    tempIconMap[ "pipe3_pipe3_v_pipe.xpm"] =                                  pipe3_pipe3_v_pipe;                        
    tempIconMap[ "pipeline_pipeline_block.xpm"] =                             pipeline_pipeline_block;                   
    tempIconMap[ "pipeline_pipeline_d_pipe.xpm"] =                            pipeline_pipeline_d_pipe;                  
    tempIconMap[ "pipeline_pipeline_hi_pipe.xpm"] =                           pipeline_pipeline_hi_pipe;                 
    tempIconMap[ "pipeline_pipeline_u_pipe.xpm"] =                            pipeline_pipeline_u_pipe;                  
    tempIconMap[ "pipeline_pipeline_v_pipe.xpm"] =                            pipeline_pipeline_v_pipe;                  
    tempIconMap[ "pipe_pipe_block.xpm"] =                                     pipe_pipe_block;                           
    tempIconMap[ "pipe_pipe_d_pipe.xpm"] =                                    pipe_pipe_d_pipe;                          
    tempIconMap[ "pipe_pipe_hi_pipe.xpm"] =                                   pipe_pipe_hi_pipe;                         
    tempIconMap[ "pipe_pipe_u_pipe.xpm"] =                                    pipe_pipe_u_pipe;                          
    tempIconMap[ "pipe_pipe_v_pipe.xpm"] =                                    pipe_pipe_v_pipe;                          
    tempIconMap[ "polyfrac_polyfrac_block.xpm"] =                             polyfrac_polyfrac_block;                   
    tempIconMap[ "polyfrac_polyfrac_furnace.xpm"] =                           polyfrac_polyfrac_furnace;                 
    tempIconMap[ "polyfrac_polyfrac_heater.xpm"] =                            polyfrac_polyfrac_heater;                  
    tempIconMap[ "polyfrac_polyfrac_h_drum.xpm"] =                            polyfrac_polyfrac_h_drum;                  
    tempIconMap[ "polyfrac_polyfrac_v_drum1.xpm"] =                           polyfrac_polyfrac_v_drum1;                 
    tempIconMap[ "polyfrac_polyfrac_v_drum2.xpm"] =                           polyfrac_polyfrac_v_drum2;                 
    tempIconMap[ "prbs_prbs_icon1.xpm"] =                                     prbs_prbs_icon1;                           
    tempIconMap[ "psv2_psv2_pfd1.xpm"] =                                      psv2_psv2_pfd1;                            
    tempIconMap[ "psv_psv_pfd1.xpm"] =                                        psv_psv_pfd1;                              
    tempIconMap[ "pump_pump_block.xpm"] =                                     pump_pump_block;                           
    tempIconMap[ "pump_pump_icon1.xpm"] =                                     pump_pump_icon1;                           
    tempIconMap[ "pump_pump_icon2.xpm"] =                                     pump_pump_icon2;                           
    tempIconMap[ "qtvec_qtvec_block.xpm"] =                                   qtvec_qtvec_block;                         
    tempIconMap[ "qtvec_qtvec_dot.xpm"] =                                     qtvec_qtvec_dot;                           
    tempIconMap[ "radfrac_radfrac_absbr1.xpm"] =                              radfrac_radfrac_absbr1;                    
    tempIconMap[ "radfrac_radfrac_absbr2.xpm"] =                              radfrac_radfrac_absbr2;                    
    tempIconMap[ "radfrac_radfrac_absbr3.xpm"] =                              radfrac_radfrac_absbr3;                    
    tempIconMap[ "radfrac_radfrac_block.xpm"] =                               radfrac_radfrac_block;                     
    tempIconMap[ "radfrac_radfrac_decant1.xpm"] =                             radfrac_radfrac_decant1;                   
    tempIconMap[ "radfrac_radfrac_decant2.xpm"] =                             radfrac_radfrac_decant2;                   
    tempIconMap[ "radfrac_radfrac_decant3.xpm"] =                             radfrac_radfrac_decant3;                   
    tempIconMap[ "radfrac_radfrac_fract1.xpm"] =                              radfrac_radfrac_fract1;                    
    tempIconMap[ "radfrac_radfrac_fract2.xpm"] =                              radfrac_radfrac_fract2;                    
    tempIconMap[ "radfrac_radfrac_packabs.xpm"] =                             radfrac_radfrac_packabs;                   
    tempIconMap[ "radfrac_radfrac_packcol1.xpm"] =                            radfrac_radfrac_packcol1;                  
    tempIconMap[ "radfrac_radfrac_packcol2.xpm"] =                            radfrac_radfrac_packcol2;                  
    tempIconMap[ "radfrac_radfrac_packstr1.xpm"] =                            radfrac_radfrac_packstr1;                  
    tempIconMap[ "radfrac_radfrac_packstr2.xpm"] =                            radfrac_radfrac_packstr2;                  
    tempIconMap[ "radfrac_radfrac_rect.xpm"] =                                radfrac_radfrac_rect;                      
    tempIconMap[ "radfrac_radfrac_strip1.xpm"] =                              radfrac_radfrac_strip1;                    
    tempIconMap[ "radfrac_radfrac_strip2.xpm"] =                              radfrac_radfrac_strip2;                    
    tempIconMap[ "ratefrac_ratefrac_absbr2.xpm"] =                            ratefrac_ratefrac_absbr2;                  
    tempIconMap[ "ratefrac_ratefrac_absbr3.xpm"] =                            ratefrac_ratefrac_absbr3;                  
    tempIconMap[ "ratefrac_ratefrac_absorber.xpm"] =                          ratefrac_ratefrac_absorber;                
    tempIconMap[ "ratefrac_ratefrac_block.xpm"] =                             ratefrac_ratefrac_block;                   
    tempIconMap[ "ratefrac_ratefrac_fract.xpm"] =                             ratefrac_ratefrac_fract;                   
    tempIconMap[ "ratefrac_ratefrac_packabs.xpm"] =                           ratefrac_ratefrac_packabs;                 
    tempIconMap[ "ratefrac_ratefrac_packcol.xpm"] =                           ratefrac_ratefrac_packcol;                 
    tempIconMap[ "ratefrac_ratefrac_packstr.xpm"] =                           ratefrac_ratefrac_packstr;                 
    tempIconMap[ "ratefrac_ratefrac_rect.xpm"] =                              ratefrac_ratefrac_rect;                    
    tempIconMap[ "ratefrac_ratefrac_stripper.xpm"] =                          ratefrac_ratefrac_stripper;                
    tempIconMap[ "ratefrac_ratefrac_vacuum1.xpm"] =                           ratefrac_ratefrac_vacuum1;                 
    tempIconMap[ "ratefrac_ratefrac_vacuum2.xpm"] =                           ratefrac_ratefrac_vacuum2;                 
    tempIconMap[ "ratio_ratio_icon1.xpm"] =                                   ratio_ratio_icon1;                         
    tempIconMap[ "rbatch_rbatch_block.xpm"] =                                 rbatch_rbatch_block;                       
    tempIconMap[ "rbatch_rbatch_icon1.xpm"] =                                 rbatch_rbatch_icon1;                       
    tempIconMap[ "rcstr2_rcstr2_block.xpm"] =                                 rcstr2_rcstr2_block;                       
    tempIconMap[ "rcstr2_rcstr2_horizontal.xpm"] =                            rcstr2_rcstr2_horizontal;                  
    tempIconMap[ "rcstr2_rcstr2_icon1.xpm"] =                                 rcstr2_rcstr2_icon1;                       
    tempIconMap[ "rcstr2_rcstr2_sperical.xpm"] =                              rcstr2_rcstr2_sperical;                    
    tempIconMap[ "rcstr2_rcstr2_vertical.xpm"] =                              rcstr2_rcstr2_vertical;                    
    tempIconMap[ "rcstr3_rcstr3_block.xpm"] =                                 rcstr3_rcstr3_block;                       
    tempIconMap[ "rcstr3_rcstr3_furnace.xpm"] =                               rcstr3_rcstr3_furnace;                     
    tempIconMap[ "rcstr3_rcstr3_heater.xpm"] =                                rcstr3_rcstr3_heater;                      
    tempIconMap[ "rcstr3_rcstr3_horizontal.xpm"] =                            rcstr3_rcstr3_horizontal;                  
    tempIconMap[ "rcstr3_rcstr3_h_drum.xpm"] =                                rcstr3_rcstr3_h_drum;                      
    tempIconMap[ "rcstr3_rcstr3_icon1.xpm"] =                                 rcstr3_rcstr3_icon1;                       
    tempIconMap[ "rcstr3_rcstr3_sperical.xpm"] =                              rcstr3_rcstr3_sperical;                    
    tempIconMap[ "rcstr3_rcstr3_vertical.xpm"] =                              rcstr3_rcstr3_vertical;                    
    tempIconMap[ "rcstr3_rcstr3_v_drum1.xpm"] =                               rcstr3_rcstr3_v_drum1;                     
    tempIconMap[ "rcstr3_rcstr3_v_drum2.xpm"] =                               rcstr3_rcstr3_v_drum2;                     
    tempIconMap[ "rcstr_rcstr_block.xpm"] =                                   rcstr_rcstr_block;                         
    tempIconMap[ "rcstr_rcstr_horizontal.xpm"] =                              rcstr_rcstr_horizontal;                    
    tempIconMap[ "rcstr_rcstr_icon1.xpm"] =                                   rcstr_rcstr_icon1;                         
    tempIconMap[ "rcstr_rcstr_sperical.xpm"] =                                rcstr_rcstr_sperical;                      
    tempIconMap[ "rcstr_rcstr_vertical.xpm"] =                                rcstr_rcstr_vertical;                      
    tempIconMap[ "requil_requil_block.xpm"] =                                 requil_requil_block;                       
    tempIconMap[ "requil_requil_icon2.xpm"] =                                 requil_requil_icon2;                       
    tempIconMap[ "requil_requil_icon3.xpm"] =                                 requil_requil_icon3;                       
    tempIconMap[ "rgibbs2_rgibbs2_block.xpm"] =                               rgibbs2_rgibbs2_block;                     
    tempIconMap[ "rgibbs2_rgibbs2_icon1.xpm"] =                               rgibbs2_rgibbs2_icon1;                     
    tempIconMap[ "rgibbs2_rgibbs2_icon2.xpm"] =                               rgibbs2_rgibbs2_icon2;                     
    tempIconMap[ "rgibbs_rgibbs_block.xpm"] =                                 rgibbs_rgibbs_block;                       
    tempIconMap[ "rgibbs_rgibbs_icon1.xpm"] =                                 rgibbs_rgibbs_icon1;                       
    tempIconMap[ "rgibbs_rgibbs_icon2.xpm"] =                                 rgibbs_rgibbs_icon2;                       
    tempIconMap[ "rplugpde_rplugpde_block.xpm"] =                             rplugpde_rplugpde_block;                   
    tempIconMap[ "rplugpde_rplugpde_icon1.xpm"] =                             rplugpde_rplugpde_icon1;                   
    tempIconMap[ "rplugpde_rplugpde_icon2.xpm"] =                             rplugpde_rplugpde_icon2;                   
    tempIconMap[ "rplugpde_rplugpde_icon3.xpm"] =                             rplugpde_rplugpde_icon3;                   
    tempIconMap[ "rplug_rplug_block.xpm"] =                                   rplug_rplug_block;                         
    tempIconMap[ "rplug_rplug_icon1.xpm"] =                                   rplug_rplug_icon1;                         
    tempIconMap[ "rplug_rplug_icon2.xpm"] =                                   rplug_rplug_icon2;                         
    tempIconMap[ "rplug_rplug_icon3.xpm"] =                                   rplug_rplug_icon3;                         
    tempIconMap[ "rstoic_rstoic_block.xpm"] =                                 rstoic_rstoic_block;                       
    tempIconMap[ "rstoic_rstoic_icon1.xpm"] =                                 rstoic_rstoic_icon1;                       
    tempIconMap[ "rstoic_rstoic_icon2.xpm"] =                                 rstoic_rstoic_icon2;                       
    tempIconMap[ "rstoic_rstoic_icon3.xpm"] =                                 rstoic_rstoic_icon3;                       
    tempIconMap[ "rstoic_rstoic_icon4.xpm"] =                                 rstoic_rstoic_icon4;                       
    tempIconMap[ "ryield_ryield_block.xpm"] =                                 ryield_ryield_block;                       
    tempIconMap[ "ryield_ryield_icon2.xpm"] =                                 ryield_ryield_icon2;                       
    tempIconMap[ "ryield_ryield_icon3.xpm"] =                                 ryield_ryield_icon3;                       
    tempIconMap[ "scale_scale_icon1.xpm"] =                                   scale_scale_icon1;                         
    tempIconMap[ "scfrac_scfrac_block.xpm"] =                                 scfrac_scfrac_block;                       
    tempIconMap[ "scfrac_scfrac_cdu1.xpm"] =                                  scfrac_scfrac_cdu1;                        
    tempIconMap[ "scfrac_scfrac_cdu2.xpm"] =                                  scfrac_scfrac_cdu2;                        
    tempIconMap[ "scfrac_scfrac_cdu3.xpm"] =                                  scfrac_scfrac_cdu3;                        
    tempIconMap[ "scfrac_scfrac_vacuum1.xpm"] =                               scfrac_scfrac_vacuum1;                     
    tempIconMap[ "scfrac_scfrac_vacuum2.xpm"] =                               scfrac_scfrac_vacuum2;                     
    tempIconMap[ "screen_screen_block.xpm"] =                                 screen_screen_block;                       
    tempIconMap[ "screen_screen_icon1.xpm"] =                                 screen_screen_icon1;                       
    tempIconMap[ "screen_screen_icon2.xpm"] =                                 screen_screen_icon2;                       
    tempIconMap[ "selector_heat_selector_heat_heat.xpm"] =                    selector_heat_selector_heat_heat;          
    tempIconMap[ "selector_selector_block.xpm"] =                             selector_selector_block;                   
    tempIconMap[ "selector_selector_dot.xpm"] =                               selector_selector_dot;                     
    tempIconMap[ "selector_selector_heat.xpm"] =                              selector_selector_heat;                    
    tempIconMap[ "selector_selector_triangle.xpm"] =                          selector_selector_triangle;                
    tempIconMap[ "selector_selector_work.xpm"] =                              selector_selector_work;                    
    tempIconMap[ "selector_work_selector_work_work.xpm"] =                    selector_work_selector_work_work;          
    tempIconMap[ "sep2_sep2_block.xpm"] =                                     sep2_sep2_block;                           
    tempIconMap[ "sep2_sep2_icon1.xpm"] =                                     sep2_sep2_icon1;                           
    tempIconMap[ "sep2_sep2_icon2.xpm"] =                                     sep2_sep2_icon2;                           
    tempIconMap[ "sep2_sep2_icon3.xpm"] =                                     sep2_sep2_icon3;                           
    tempIconMap[ "sep_sep_block.xpm"] =                                       sep_sep_block;                             
    tempIconMap[ "sep_sep_icon1.xpm"] =                                       sep_sep_icon1;                             
    tempIconMap[ "sep_sep_icon2.xpm"] =                                       sep_sep_icon2;                             
    tempIconMap[ "sep_sep_icon3.xpm"] =                                       sep_sep_icon3;                             
    tempIconMap[ "signalgenerator_signalgenerator_icon1.xpm"] =               signalgenerator_signalgenerator_icon1;     
    tempIconMap[ "signalgenerator_signalgenerator_icon2.xpm"] =               signalgenerator_signalgenerator_icon2;     
    tempIconMap[ "signalselector_signalselector_icon1.xpm"] =                 signalselector_signalselector_icon1;       
    tempIconMap[ "splitrange_splitrange_icon1.xpm"] =                         splitrange_splitrange_icon1;               
    tempIconMap[ "ssplit_ssplit_3way.xpm"] =                                  ssplit_ssplit_3way;                        
    tempIconMap[ "ssplit_ssplit_block.xpm"] =                                 ssplit_ssplit_block;                       
    tempIconMap[ "ssplit_ssplit_ccd.xpm"] =                                   ssplit_ssplit_ccd;                         
    tempIconMap[ "ssplit_ssplit_cfuge.xpm"] =                                 ssplit_ssplit_cfuge;                       
    tempIconMap[ "ssplit_ssplit_cyclone.xpm"] =                               ssplit_ssplit_cyclone;                     
    tempIconMap[ "ssplit_ssplit_dot.xpm"] =                                   ssplit_ssplit_dot;                         
    tempIconMap[ "ssplit_ssplit_filter1.xpm"] =                               ssplit_ssplit_filter1;                     
    tempIconMap[ "ssplit_ssplit_filter2.xpm"] =                               ssplit_ssplit_filter2;                     
    tempIconMap[ "ssplit_ssplit_screen.xpm"] =                                ssplit_ssplit_screen;                      
    tempIconMap[ "ssplit_ssplit_tee.xpm"] =                                   ssplit_ssplit_tee;                         
    tempIconMap[ "ssplit_ssplit_triangle.xpm"] =                              ssplit_ssplit_triangle;                    
    tempIconMap[ "ssplit_ssplit_vscrub.xpm"] =                                ssplit_ssplit_vscrub;                      
    tempIconMap[ "standalone_standalone_standalone.xpm"] =                    standalone_standalone_standalone;                                
    tempIconMap[ "steamptot_steamptot_icon1.xpm"] =                           steamptot_steamptot_icon1;                 
    tempIconMap[ "steamttop_steamttop_icon1.xpm"] =                           steamttop_steamttop_icon1;                 
    tempIconMap[ "sum_sum_icon1.xpm"] =                                       sum_sum_icon1;                             
    tempIconMap[ "swash_swash_block.xpm"] =                                   swash_swash_block;                         
    tempIconMap[ "swash_swash_icon.xpm"] =                                    swash_swash_icon;                          
    tempIconMap[ "teemixer_teemixer_dot.xpm"] =                               teemixer_teemixer_dot;                     
    tempIconMap[ "teemixer_teemixer_tee.xpm"] =                               teemixer_teemixer_tee;                     
    tempIconMap[ "teesplitter_teesplitter_dot.xpm"] =                         teesplitter_teesplitter_dot;               
    tempIconMap[ "teesplitter_teesplitter_tee.xpm"] =                         teesplitter_teesplitter_tee;               
    tempIconMap[ "timedata_timedata_timedata.xpm"] =                          timedata_timedata_timedata;                
    tempIconMap[ "transform_transform_icon1.xpm"] =                           transform_transform_icon1;                 
    tempIconMap[ "tvalve_tvalve_tval1.xpm"] =                                 tvalve_tvalve_tval1;                       
    tempIconMap[ "user2_user2_block.xpm"] =                                   user2_user2_block;                         
    tempIconMap[ "user2_user2_cfuge.xpm"] =                                   user2_user2_cfuge;                         
    tempIconMap[ "user2_user2_cstr.xpm"] =                                    user2_user2_cstr;                          
    tempIconMap[ "user2_user2_excel.xpm"] =                                   user2_user2_excel;                         
    tempIconMap[ "user2_user2_filter.xpm"] =                                  user2_user2_filter;                        
    tempIconMap[ "user2_user2_fract.xpm"] =                                   user2_user2_fract;                         
    tempIconMap[ "user2_user2_heater.xpm"] =                                  user2_user2_heater;                        
    tempIconMap[ "user2_user2_heatx1.xpm"] =                                  user2_user2_heatx1;                        
    tempIconMap[ "user2_user2_heatx2.xpm"] =                                  user2_user2_heatx2;                        
    tempIconMap[ "user2_user2_h_drum.xpm"] =                                  user2_user2_h_drum;                        
    tempIconMap[ "user2_user2_plug.xpm"] =                                    user2_user2_plug;                          
    tempIconMap[ "user2_user2_reactor.xpm"] =                                 user2_user2_reactor;                       
    tempIconMap[ "user2_user2_rect.xpm"] =                                    user2_user2_rect;                          
    tempIconMap[ "user2_user2_strip.xpm"] =                                   user2_user2_strip;                         
    tempIconMap[ "user2_user2_valve4.xpm"] =                                  user2_user2_valve4;                        
    tempIconMap[ "user2_user2_v_drum.xpm"] =                                  user2_user2_v_drum;                        
    tempIconMap[ "user3_user3_block.xpm"] =                                   user3_user3_block;                         
    tempIconMap[ "user3_user3_cfuge.xpm"] =                                   user3_user3_cfuge;                         
    tempIconMap[ "user3_user3_cstr.xpm"] =                                    user3_user3_cstr;                          
    tempIconMap[ "user3_user3_filter.xpm"] =                                  user3_user3_filter;                        
    tempIconMap[ "user3_user3_fract.xpm"] =                                   user3_user3_fract;                         
    tempIconMap[ "user3_user3_heater.xpm"] =                                  user3_user3_heater;                        
    tempIconMap[ "user3_user3_heatx1.xpm"] =                                  user3_user3_heatx1;                        
    tempIconMap[ "user3_user3_heatx2.xpm"] =                                  user3_user3_heatx2;                        
    tempIconMap[ "user3_user3_h_drum.xpm"] =                                  user3_user3_h_drum;                        
    tempIconMap[ "user3_user3_plug.xpm"] =                                    user3_user3_plug;                          
    tempIconMap[ "user3_user3_reactor.xpm"] =                                 user3_user3_reactor;                       
    tempIconMap[ "user3_user3_rect.xpm"] =                                    user3_user3_rect;                          
    tempIconMap[ "user3_user3_strip.xpm"] =                                   user3_user3_strip;                         
    tempIconMap[ "user3_user3_valve4.xpm"] =                                  user3_user3_valve4;                        
    tempIconMap[ "user3_user3_v_drum.xpm"] =                                  user3_user3_v_drum;                        
    tempIconMap[ "user_user_block.xpm"] =                                     user_user_block;                           
    tempIconMap[ "user_user_small.xpm"] =                                     user_user_small;                           
    tempIconMap[ "valve_valve_block.xpm"] =                                   valve_valve_block;                         
    tempIconMap[ "valve_valve_icon1.xpm"] =                                   valve_valve_icon1;                         
    tempIconMap[ "valve_valve_valve1.xpm"] =                                  valve_valve_valve1;                        
    tempIconMap[ "valve_valve_valve2.xpm"] =                                  valve_valve_valve2;                        
    tempIconMap[ "valve_valve_valve3.xpm"] =                                  valve_valve_valve3;                        
    tempIconMap[ "valve_valve_valve4.xpm"] =                                  valve_valve_valve4;                        
    tempIconMap[ "vscrub_vscrub_block.xpm"] =                                 vscrub_vscrub_block;                       
    tempIconMap[ "vscrub_vscrub_icon.xpm"] =                                  vscrub_vscrub_icon;                        
    
    return tempIconMap;
}