#include <ves/conductor/AspenPlus2DIcons.h>

#include <map>
#include <string>

#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_v_drum1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/decanter_decanter_h_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_sperical.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ise_ise_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_heat_dupl_heat_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_work_fsplit_work_triangle.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_packcol.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_v_drum1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_heater.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_ecn_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_ecn_hs_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_g_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j12_hs2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j12_hs2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_x_ht_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/swash_swash_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_filter.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/deltap_deltap_dp.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_cstr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_mult_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_g_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu7.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dmcplus_dmcplus_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_x_hs_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_g_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_ht_2cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/qtvec_qtvec_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_vacuum2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_hs_2cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_petlyuk.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_filter.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_icon3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/prbs_prbs_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_strip.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_smp_ht.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/batchfrac_batchfrac_column2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_valve.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_d_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/signalgenerator_signalgenerator_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dstwu_dstwu_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_x_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_h_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_hs_1co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/transform_transform_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_plug.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_vscrub.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_heat_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/batchfrac_batchfrac_column3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_preflash.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/kineticsest_kineticsest_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/analyzer_analyzer_analyze2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_work_dupl_work_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_horizontal.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/screen_screen_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu11f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_valve4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_v_drum2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dmcplus_dmcplus_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_dupl_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_rect.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_hs_1co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multisum_multisum_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_ht_2co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/steamttop_steamttop_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_work_selector_work_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/esp_esp_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aplusproduct_aplusproduct_arrow.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu5.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j12_ht1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_natura_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_furnace.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/esp_esp_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/compr_compr_icon3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu8f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_plug.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j12_hs1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_gen_ht.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu6.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_x_ht_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_smp_ht.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/feedforward_feedforward_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_filter2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/expansion_expansion_expand1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j21_ht1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/batchfrac_batchfrac_column1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_v_drum2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu7.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_cstr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sep2_sep2_icon3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/scale_scale_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_cfuge.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_h_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j12_ht2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/analyzer_analyzer_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/signalselector_signalselector_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hycyc_hycyc_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_h_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_h_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/analyzer_analyzer_analyzer.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_induce_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rplugpde_rplugpde_icon3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j12_ht1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_g_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_aircol.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/compr_compr_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sep2_sep2_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_cocurnt.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_rect.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_heater.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_work_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_heater.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dryer_dryer_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_furnace.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs2_rgibbs2_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure7.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_fcc_mf2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_tindictr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dryer_dryer_spray.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_v_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j12_hs1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_cfuge.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j12_hs2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_x_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu5.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_hs_2cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fabfl_fabfl_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_ht_1cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_heater.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/extract_extract_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j21_ht2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packcol2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_ht_2cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j21_hs1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/filter_filter_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rplugpde_rplugpde_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_simp_mhx.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu8.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_x_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/orifice_orifice_orifice1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/steamptot_steamptot_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_fcc_mf1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_ht_1co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_counter.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_h_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_v_drum1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_vacuum1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu2f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_fcontlr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j21_hs1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/distl_distl_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_prefl1f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/extract_extract_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sep_sep_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_absbr1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_k_ht_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu9f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_filter1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_forced_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/crystallizer_crystallizer_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_valve2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_simp_hs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu15f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu15.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_k_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_packstr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_natura_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_cfuge.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_v_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_simp_ht.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_block2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_vacuum1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_fract.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/crusher_crusher_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_prefl2f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dstwu_dstwu_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_ccd.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_triangle.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/swash_swash_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_compr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_induce_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/standalone.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_h_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/extract_extract_pod.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mcompr_mcompr_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_h_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_decant1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs2_rgibbs2_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu6f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_ht_1cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu12.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/distl_distl_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_x_ht_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_ht_4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j12_ht2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/clchng_clchng_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_hi_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_dupl_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/noise_noise_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_gen_ht.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_findictr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_dupl_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pidincr_pidincr_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/screen_screen_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_vertical.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dynamics.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dstwu_dstwu_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user_user_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_vacuum2f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_h_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/splitrange_splitrange_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_strip.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_x_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_g_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/timedata_timedata_timedata.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_fract.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_contract.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu5f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_v_drum2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/logicgate_logicgate_logicgate.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_hs_2co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu9.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rbatch_rbatch_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j12_ht1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pid_pid_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/burstingdisk_burstingdisk_bdisk1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j21_hs2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_vacuum1f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/cyclone_cyclone_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_prefl1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_work_mult_work_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sep2_sep2_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rplug_rplug_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_reactor.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_hs_4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_horizontal.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_sperical.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_vacuum2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/teesplitter_teesplitter_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_vacuum2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_ecn_hs_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_gen_hs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rplug_rplug_icon3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_v_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dryer_dryer_fluidbed2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hxflux_hxflux_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_packabs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j12_ht2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_decant3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aplusfeed_aplusfeed_arrow.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_h_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_x_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_heatx1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/logiccompare_logiccompare_logiccompare.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_cdu3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/comparator_comparator_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_block3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_heat_mult_heat_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_screen.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_3way.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_v_drum1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs_rgibbs_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rplugpde_rplugpde_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j12_hs1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_simp_hs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/compr_compr_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/requil_requil_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_heatx1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_valve1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratio_ratio_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_vacuum2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multiply_multiply_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/filter_filter_plate.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_simp_hs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j21_hs2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_vertical.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j21_ht1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/requil_requil_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr_rcstr_sperical.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs_rgibbs_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_ecn_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packstr1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_ecn_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu12f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_triangle.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_e_hs_1cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_pcontlr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_gen_hs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_forced_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hierarchy_hierarchy_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pump_pump_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_expand.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_d_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_induce_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_rect.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_counter2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/psv_psv_pfd1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_hs_2co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/apecs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/feedbl_feedbl_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs_rgibbs_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user_user_small.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sum_sum_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sep_sep_icon3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/decanter_decanter_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_g_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_mult_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/tvalve_tvalve_tval1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure5.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_u_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_hs_2cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_acontlr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_heat_mixer_heat_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_cdu2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_heat_fsplit_heat_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_x_hs_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_lindictr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_forced_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_d_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_heatx2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_h_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_v_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_vacuum1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_ecn_ht_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/teemixer_teemixer_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_excel.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_vertical.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu14.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_hs_1cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/crystallizer_crystallizer_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_u_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/filter_filter_rotary.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j21_ht2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sep_sep_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_b_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_mult_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_pump.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_expand.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packstr2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_pindictr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j21_hs1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_d_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ccd_ccd_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_j21_ht1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure6.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_hs_4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/screen_screen_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/expansion_expansion_contract1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_valve4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_natura_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/requil_requil_icon3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/vscrub_vscrub_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_absorber.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_x_hs_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_strip1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dupl_dupl_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu10f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/clchng_clchng_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_hs_4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_pfracf.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_heatx2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_fract.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_reactor.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_ht_4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_prefl2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu1f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_icon4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_tank.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/valve_valve_valve3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_valve4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_ecn_ht_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/lag_1_lag_1_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_fract2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_horizontal.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multihiloselect_multihiloselect_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_furnace.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_f_ht_4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash3_flash3_heater.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_ecn_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_heater.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/teesplitter_teesplitter_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aspen.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_heat_selector_heat_heat.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/extract_extract_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_fract.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/batchfrac_batchfrac_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu13.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/psv2_psv2_pfd1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/teemixer_teemixer_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_hs_1co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_furnace.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/cfuge_cfuge_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_absbr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/distl_distl_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ryield_ryield_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_triangle.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_vacuum1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dicretize_discretize_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pump_pump_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fabfl_fabfl_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_heat_fsplit_heat_triangle.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu13f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ryield_ryield_icon3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_v_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe_pipe_u_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu9.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_k_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mheatx_mheatx_circ_mhx.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_sperical.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/flash2_flash2_heater.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_aircooler.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_valve4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_work_fsplit_work_work_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_vertical.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_cdu6.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_forced_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dryer_dryer_fluidbed.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure4.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_hs_2co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/selector_selector_dot.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/crusher_crusher_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_pfrac.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_fract1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu10.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_hopper.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_hs_1cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_f_ht_2co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_absbr2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sep_sep_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mult_mult_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/sep2_sep2_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rbatch_rbatch_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_contract.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/cyclone_cyclone_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_lcontlr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/dead_time_dead_time_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_ht_1co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rplug_rplug_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_h_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu8.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hiloselect_hiloselect_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hycyc_hycyc_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pump_pump_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_heat_fsplit_heat_heat_tee.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_e_ht_1cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_j21_ht2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_valve2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_decant2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mcompr_mcompr_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/decanter_decanter_v_drum.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_ht_2co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_v_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_triangle.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_measure8.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/crystallizer_crystallizer_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipeline_pipeline_hi_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_absbr2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_k_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_v_drum2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu14f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_strip2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe3_pipe3_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/feedbl_feedbl_feedbl.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_aindictr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_h_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu7f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu4f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rplug_rplug_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ssplit_ssplit_cyclone.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packcol1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/signalgenerator_signalgenerator_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/compr_compr_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_f_ht_2cn.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_work_mixer_work_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_packabs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/cfuge_cfuge_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rplugpde_rplugpde_icon2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_natura_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_absbr3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/scfrac_scfrac_cdu1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rstoic_rstoic_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_j21_hs2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_h_hs_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_rect.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_3way.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_e_ht_1co.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/radfrac_radfrac_absbr3.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/htrixist_htrixist_gen_hs.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu3f.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_u_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heater_heater_valve.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_k_ht_1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user2_user2_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_gen_ht.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/aerotran_aerotran_induce_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ryield_ryield_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ccd_ccd_icon.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/qtvec_qtvec_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/heatx_heatx_x_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/pipe2_pipe2_b_pipe.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/measurement_measurement_tcontlr.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu11.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_work_fsplit_work_work.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/hetran_hetran_h_ht_2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/iae_iae_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/mixer_mixer_screw.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/petrofrac_petrofrac_cdu2.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/polyfrac_polyfrac_furnace.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/multifrac_multifrac_pfrac.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rgibbs2_rgibbs2_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/fsplit_fsplit_3way.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/ratefrac_ratefrac_stripper.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/user3_user3_strip.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr2_rcstr2_horizontal.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/lead_lag_lead_lag_icon1.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/rcstr3_rcstr3_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/vscrub_vscrub_block.xpm>
#include <ves/conductor/xpm/AspenPlus2DIcons/onoffcontrol_onoffcontrol_digitalpoint.xpm>



std::map< std::string, char** > GetAspenPlusIconMap()
{
    std::map< std::string, char** > tempIconMap;

    tempIconMap[ "2dicons/esp/esp.block.jpg" ] =                                         esp_esp_block;
    tempIconMap[ "2dicons/esp/esp.icon.jpg" ] =                                          esp_esp_icon;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu3.jpg" ] =                              multifrac_multifrac_cdu3;
    tempIconMap[ "2dicons/multifrac/multifrac.vacuum1.jpg" ] =                           multifrac_multifrac_vacuum1;
    tempIconMap[ "2dicons/multifrac/multifrac.aircol.jpg" ] =                            multifrac_multifrac_aircol;
    tempIconMap[ "2dicons/multifrac/multifrac.pfrac.jpg" ] =                             multifrac_multifrac_pfrac;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu2.jpg" ] =                              multifrac_multifrac_cdu2;
    tempIconMap[ "2dicons/multifrac/multifrac.block.jpg" ] =                             multifrac_multifrac_block;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu1.jpg" ] =                              multifrac_multifrac_cdu1;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu9.jpg" ] =                              multifrac_multifrac_cdu9;
    tempIconMap[ "2dicons/multifrac/multifrac.vacuum2.jpg" ] =                           multifrac_multifrac_vacuum2;
    tempIconMap[ "2dicons/multifrac/multifrac.preflash.jpg" ] =                          multifrac_multifrac_preflash;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu6.jpg" ] =                              multifrac_multifrac_cdu6;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu7.jpg" ] =                              multifrac_multifrac_cdu7;
    tempIconMap[ "2dicons/multifrac/multifrac.petlyuk.jpg" ] =                           multifrac_multifrac_petlyuk;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu8.jpg" ] =                              multifrac_multifrac_cdu8;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu4.jpg" ] =                              multifrac_multifrac_cdu4;
    tempIconMap[ "2dicons/multifrac/multifrac.cdu5.jpg" ] =                              multifrac_multifrac_cdu5;
    tempIconMap[ "2dicons/ratefrac/ratefrac.packabs.jpg" ] =                             ratefrac_ratefrac_packabs;
    tempIconMap[ "2dicons/ratefrac/ratefrac.block.jpg" ] =                               ratefrac_ratefrac_block;
    tempIconMap[ "2dicons/ratefrac/ratefrac.vacuum2.jpg" ] =                             ratefrac_ratefrac_vacuum2;
    tempIconMap[ "2dicons/ratefrac/ratefrac.absbr3.jpg" ] =                              ratefrac_ratefrac_absbr3;
    tempIconMap[ "2dicons/ratefrac/ratefrac.absbr2.jpg" ] =                              ratefrac_ratefrac_absbr2;
    tempIconMap[ "2dicons/ratefrac/ratefrac.packcol.jpg" ] =                             ratefrac_ratefrac_packcol;
    tempIconMap[ "2dicons/ratefrac/ratefrac.absorber.jpg" ] =                            ratefrac_ratefrac_absorber;
    tempIconMap[ "2dicons/ratefrac/ratefrac.stripper.jpg" ] =                            ratefrac_ratefrac_stripper;
    tempIconMap[ "2dicons/ratefrac/ratefrac.fract.jpg" ] =                               ratefrac_ratefrac_fract;
    tempIconMap[ "2dicons/ratefrac/ratefrac.rect.jpg" ] =                                ratefrac_ratefrac_rect;
    tempIconMap[ "2dicons/ratefrac/ratefrac.vacuum1.jpg" ] =                             ratefrac_ratefrac_vacuum1;
    tempIconMap[ "2dicons/ratefrac/ratefrac.packstr.jpg" ] =                             ratefrac_ratefrac_packstr;
    tempIconMap[ "2dicons/cfuge/cfuge.icon.jpg" ] =                                      cfuge_cfuge_icon;
    tempIconMap[ "2dicons/cfuge/cfuge.block.jpg" ] =                                     cfuge_cfuge_block;
    tempIconMap[ "2dicons/decanter/decanter.block.jpg" ] =                               decanter_decanter_block;
    tempIconMap[ "2dicons/decanter/decanter_v_drum.jpg" ] =                              decanter_decanter_v_drum;
    tempIconMap[ "2dicons/decanter/decanter.h_drum.jpg" ] =                              decanter_decanter_h_drum;
    tempIconMap[ "2dicons/measurement/measurement.fcontlr.jpg" ] =                       measurement_measurement_fcontlr;
    tempIconMap[ "2dicons/measurement/measurement.measure3.jpg" ] =                      measurement_measurement_measure3;
    tempIconMap[ "2dicons/measurement/measurement.block.jpg" ] =                         measurement_measurement_block;
    tempIconMap[ "2dicons/measurement/measurement.lindictr.jpg" ] =                      measurement_measurement_lindictr;
    tempIconMap[ "2dicons/measurement/measurement.measure5.jpg" ] =                      measurement_measurement_measure5;
    tempIconMap[ "2dicons/measurement/measurement.measure2.jpg" ] =                      measurement_measurement_measure2;
    tempIconMap[ "2dicons/measurement/measurement.pindictr.jpg" ] =                      measurement_measurement_pindictr;
    tempIconMap[ "2dicons/measurement/measurement.tindictr.jpg" ] =                      measurement_measurement_tindictr;
    tempIconMap[ "2dicons/measurement/measurement.acontlr.jpg" ] =                       measurement_measurement_acontlr;
    tempIconMap[ "2dicons/measurement/measurement.tcontlr.jpg" ] =                       measurement_measurement_tcontlr;
    tempIconMap[ "2dicons/measurement/measurement.measure8.jpg" ] =                      measurement_measurement_measure8;
    tempIconMap[ "2dicons/measurement/measurement.measure4.jpg" ] =                      measurement_measurement_measure4;
    tempIconMap[ "2dicons/measurement/measurement.measure6.jpg" ] =                      measurement_measurement_measure6;
    tempIconMap[ "2dicons/measurement/measurement.aindictr.jpg" ] =                      measurement_measurement_aindictr;
    tempIconMap[ "2dicons/measurement/measurement.measure7.jpg" ] =                      measurement_measurement_measure7;
    tempIconMap[ "2dicons/measurement/measurement.lcontlr.jpg" ] =                       measurement_measurement_lcontlr;
    tempIconMap[ "2dicons/measurement/measurement.findictr.jpg" ] =                      measurement_measurement_findictr;
    tempIconMap[ "2dicons/measurement/measurement.measure1.jpg" ] =                      measurement_measurement_measure1;
    tempIconMap[ "2dicons/measurement/measurement.pcontlr.jpg" ] =                       measurement_measurement_pcontlr;
    tempIconMap[ "2dicons/user3/user3.rect.jpg" ] =                                      user3_user3_rect;
    tempIconMap[ "2dicons/user3/user3.reactor.jpg" ] =                                   user3_user3_reactor;
    tempIconMap[ "2dicons/user3/user3.plug.jpg" ] =                                      user3_user3_plug;
    tempIconMap[ "2dicons/user3/user3.cfuge.jpg" ] =                                     user3_user3_cfuge;
    tempIconMap[ "2dicons/user3/user3.v_drum.jpg" ] =                                    user3_user3_v_drum;
    tempIconMap[ "2dicons/user3/user3.heatx1.jpg" ] =                                    user3_user3_heatx1;
    tempIconMap[ "2dicons/user3/user3.filter.jpg" ] =                                    user3_user3_filter;
    tempIconMap[ "2dicons/user3/user3.block.jpg" ] =                                     user3_user3_block;
    tempIconMap[ "2dicons/user3/user3.fract.jpg" ] =                                     user3_user3_fract;
    tempIconMap[ "2dicons/user3/user3.heater.jpg" ] =                                    user3_user3_heater;
    tempIconMap[ "2dicons/user3/user3.valve4.jpg" ] =                                    user3_user3_valve4;
    tempIconMap[ "2dicons/user3/user3.h_drum.jpg" ] =                                    user3_user3_h_drum;
    tempIconMap[ "2dicons/user3/user3.cstr.jpg" ] =                                      user3_user3_cstr;
    tempIconMap[ "2dicons/user3/user3.strip.jpg" ] =                                     user3_user3_strip;
    tempIconMap[ "2dicons/user3/user3.heatx2.jpg" ] =                                    user3_user3_heatx2;
    tempIconMap[ "2dicons/hxflux/hxflux.block.jpg" ] =                                   hxflux_hxflux_block;
    tempIconMap[ "2dicons/dstwu/dstwu.block.jpg" ] =                                     dstwu_dstwu_block;
    tempIconMap[ "2dicons/dstwu/dstwu.icon1.jpg" ] =                                     dstwu_dstwu_icon1;
    tempIconMap[ "2dicons/dstwu/dstwu.icon2.jpg" ] =                                     dstwu_dstwu_icon2;
    tempIconMap[ "2dicons/rstoic/rstoic.block.jpg" ] =                                   rstoic_rstoic_block;
    tempIconMap[ "2dicons/rstoic/rstoic.icon3.jpg" ] =                                   rstoic_rstoic_icon3;
    tempIconMap[ "2dicons/rstoic/rstoic.icon1.jpg" ] =                                   rstoic_rstoic_icon1;
    tempIconMap[ "2dicons/rstoic/rstoic.icon4.jpg" ] =                                   rstoic_rstoic_icon4;
    tempIconMap[ "2dicons/rstoic/rstoic.icon2.jpg" ] =                                   rstoic_rstoic_icon2;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu9.jpg" ] =                              petrofrac_petrofrac_cdu9;
    tempIconMap[ "2dicons/petrofrac/petrofrac.prefl2.jpg" ] =                            petrofrac_petrofrac_prefl2;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu13f.jpg" ] =                            petrofrac_petrofrac_cdu13f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu1.jpg" ] =                              petrofrac_petrofrac_cdu1;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu6f.jpg" ] =                             petrofrac_petrofrac_cdu6f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.fcc_mf1.jpg" ] =                           petrofrac_petrofrac_fcc_mf1;
    tempIconMap[ "2dicons/petrofrac/petrofrac.vacuum1f.jpg" ] =                          petrofrac_petrofrac_vacuum1f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu4.jpg" ] =                              petrofrac_petrofrac_cdu4;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu7.jpg" ] =                              petrofrac_petrofrac_cdu7;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu2.jpg" ] =                              petrofrac_petrofrac_cdu2;
    tempIconMap[ "2dicons/petrofrac/petrofrac.fcc_mf2.jpg" ] =                           petrofrac_petrofrac_fcc_mf2;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu11f.jpg" ] =                            petrofrac_petrofrac_cdu11f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu12.jpg" ] =                             petrofrac_petrofrac_cdu12;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu15f.jpg" ] =                            petrofrac_petrofrac_cdu15f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.pfrac.jpg" ] =                             petrofrac_petrofrac_pfrac;
    tempIconMap[ "2dicons/petrofrac/petrofrac.fract.jpg" ] =                             petrofrac_petrofrac_fract;
    tempIconMap[ "2dicons/petrofrac/petrofrac.vacuum2f.jpg" ] =                          petrofrac_petrofrac_vacuum2f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.prefl2f.jpg" ] =                           petrofrac_petrofrac_prefl2f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.vacuum1.jpg" ] =                           petrofrac_petrofrac_vacuum1;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu11.jpg" ] =                             petrofrac_petrofrac_cdu11;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu9f.jpg" ] =                             petrofrac_petrofrac_cdu9f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.prefl1.jpg" ] =                            petrofrac_petrofrac_prefl1;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu10.jpg" ] =                             petrofrac_petrofrac_cdu10;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu14.jpg" ] =                             petrofrac_petrofrac_cdu14;
    tempIconMap[ "2dicons/petrofrac/petrofrac.prefl1f.jpg" ] =                           petrofrac_petrofrac_prefl1f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu8.jpg" ] =                              petrofrac_petrofrac_cdu8;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu12f.jpg" ] =                            petrofrac_petrofrac_cdu12f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.vacuum2.jpg" ] =                           petrofrac_petrofrac_vacuum2;
    tempIconMap[ "2dicons/petrofrac/petrofrac.block.jpg" ] =                             petrofrac_petrofrac_block;
    tempIconMap[ "2dicons/petrofrac/petrofrac.pfracf.jpg" ] =                            petrofrac_petrofrac_pfracf;
    tempIconMap[ "2dicons/petrofrac/petrofrac.absbr.jpg" ] =                             petrofrac_petrofrac_absbr;
    tempIconMap[ "2dicons/petrofrac/petrofrac.strip.jpg" ] =                             petrofrac_petrofrac_strip;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu5f.jpg" ] =                             petrofrac_petrofrac_cdu5f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu8f.jpg" ] =                             petrofrac_petrofrac_cdu8f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu10f.jpg" ] =                            petrofrac_petrofrac_cdu10f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu14f.jpg" ] =                            petrofrac_petrofrac_cdu14f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu3.jpg" ] =                              petrofrac_petrofrac_cdu3;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu5.jpg" ] =                              petrofrac_petrofrac_cdu5;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu2f.jpg" ] =                             petrofrac_petrofrac_cdu2f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu15.jpg" ] =                             petrofrac_petrofrac_cdu15;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu7f.jpg" ] =                             petrofrac_petrofrac_cdu7f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu1f.jpg" ] =                             petrofrac_petrofrac_cdu1f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu3f.jpg" ] =                             petrofrac_petrofrac_cdu3f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu6.jpg" ] =                              petrofrac_petrofrac_cdu6;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu4f.jpg" ] =                             petrofrac_petrofrac_cdu4f;
    tempIconMap[ "2dicons/petrofrac/petrofrac.cdu13.jpg" ] =                             petrofrac_petrofrac_cdu13;
    tempIconMap[ "2dicons/htrixist/htrixist.x_ht_1.jpg" ] =                              htrixist_htrixist_x_ht_1;
    tempIconMap[ "2dicons/htrixist/htrixist.simp_hs.jpg" ] =                             htrixist_htrixist_simp_hs;
    tempIconMap[ "2dicons/htrixist/htrixist.e_ht_1cn.jpg" ] =                            htrixist_htrixist_e_ht_1cn;
    tempIconMap[ "2dicons/htrixist/htrixist.j12_hs1.jpg" ] =                             htrixist_htrixist_j12_hs1;
    tempIconMap[ "2dicons/htrixist/htrixist.gen_ht.jpg" ] =                              htrixist_htrixist_gen_ht;
    tempIconMap[ "2dicons/htrixist/htrixist.f_hs_2cn.jpg" ] =                            htrixist_htrixist_f_hs_2cn;
    tempIconMap[ "2dicons/htrixist/htrixist.gen_hs.jpg" ] =                              htrixist_htrixist_gen_hs;
    tempIconMap[ "2dicons/htrixist/htrixist.block.jpg" ] =                               htrixist_htrixist_block;
    tempIconMap[ "2dicons/htrixist/htrixist.x_hs_2.jpg" ] =                              htrixist_htrixist_x_hs_2;
    tempIconMap[ "2dicons/htrixist/htrixist.h_hs_2.jpg" ] =                              htrixist_htrixist_h_hs_2;
    tempIconMap[ "2dicons/htrixist/htrixist.j21_ht1.jpg" ] =                             htrixist_htrixist_j21_ht1;
    tempIconMap[ "2dicons/htrixist/htrixist.smp_ht.jpg" ] =                              htrixist_htrixist_smp_ht;
    tempIconMap[ "2dicons/htrixist/htrixist.x_hs_1.jpg" ] =                              htrixist_htrixist_x_hs_1;
    tempIconMap[ "2dicons/htrixist/htrixist.f_ht_4.jpg" ] =                              htrixist_htrixist_f_ht_4;
    tempIconMap[ "2dicons/htrixist/htrixist.x_ht_2.jpg" ] =                              htrixist_htrixist_x_ht_2;
    tempIconMap[ "2dicons/htrixist/htrixist.j12_ht1.jpg" ] =                             htrixist_htrixist_j12_ht1;
    tempIconMap[ "2dicons/htrixist/htrixist.k_ht_1.jpg" ] =                              htrixist_htrixist_k_ht_1;
    tempIconMap[ "2dicons/htrixist/htrixist.f_hs_4.jpg" ] =                              htrixist_htrixist_f_hs_4;
    tempIconMap[ "2dicons/htrixist/htrixist.e_hs_1co.jpg" ] =                            htrixist_htrixist_e_hs_1co;
    tempIconMap[ "2dicons/htrixist/htrixist.g_hs_2.jpg" ] =                              htrixist_htrixist_g_hs_2;
    tempIconMap[ "2dicons/htrixist/htrixist.j21_hs1.jpg" ] =                             htrixist_htrixist_j21_hs1;
    tempIconMap[ "2dicons/htrixist/htrixist.j21_ht2.jpg" ] =                             htrixist_htrixist_j21_ht2;
    tempIconMap[ "2dicons/htrixist/htrixist.e_hs_1cn.jpg" ] =                            htrixist_htrixist_e_hs_1cn;
    tempIconMap[ "2dicons/htrixist/htrixist.f_hs_2co.jpg" ] =                            htrixist_htrixist_f_hs_2co;
    tempIconMap[ "2dicons/htrixist/htrixist.k_ht_2.jpg" ] =                              htrixist_htrixist_k_ht_2;
    tempIconMap[ "2dicons/htrixist/htrixist.f_ht_2co.jpg" ] =                            htrixist_htrixist_f_ht_2co;
    tempIconMap[ "2dicons/htrixist/htrixist.e_hs_2.jpg" ] =                              htrixist_htrixist_e_hs_2;
    tempIconMap[ "2dicons/htrixist/htrixist.e_ht_1co.jpg" ] =                            htrixist_htrixist_e_ht_1co;
    tempIconMap[ "2dicons/htrixist/htrixist.h_ht_2.jpg" ] =                              htrixist_htrixist_h_ht_2;
    tempIconMap[ "2dicons/htrixist/htrixist.g_ht_2.jpg" ] =                              htrixist_htrixist_g_ht_2;
    tempIconMap[ "2dicons/htrixist/htrixist.e_ht_2.jpg" ] =                              htrixist_htrixist_e_ht_2;
    tempIconMap[ "2dicons/htrixist/htrixist.j12_ht2.jpg" ] =                             htrixist_htrixist_j12_ht2;
    tempIconMap[ "2dicons/htrixist/htrixist.f_ht_2cn.jpg" ] =                            htrixist_htrixist_f_ht_2cn;
    tempIconMap[ "2dicons/htrixist/htrixist.j21_hs2.jpg" ] =                             htrixist_htrixist_j21_hs2;
    tempIconMap[ "2dicons/htrixist/htrixist.j12_hs2.jpg" ] =                             htrixist_htrixist_j12_hs2;
    tempIconMap[ "2dicons/requil/requil.icon3.jpg" ] =                                   requil_requil_icon3;
    tempIconMap[ "2dicons/requil/requil.block.jpg" ] =                                   requil_requil_block;
    tempIconMap[ "2dicons/requil/requil.icon2.jpg" ] =                                   requil_requil_icon2;
    tempIconMap[ "2dicons/feedbl/feedbl.feedbl.jpg" ] =                                  feedbl_feedbl_feedbl;
    tempIconMap[ "2dicons/feedbl/feedbl.block.jpg" ] =                                   feedbl_feedbl_block;
    tempIconMap[ "2dicons/heatx/heatx.k_ht_2.jpg" ] =                                    heatx_heatx_k_ht_2;
    tempIconMap[ "2dicons/heatx/heatx.j12_hs2.jpg" ] =                                   heatx_heatx_j12_hs2;
    tempIconMap[ "2dicons/heatx/heatx.x_ht_2.jpg" ] =                                    heatx_heatx_x_ht_2;
    tempIconMap[ "2dicons/heatx/heatx.gen_ht.jpg" ] =                                    heatx_heatx_gen_ht;
    tempIconMap[ "2dicons/heatx/heatx.j21_hs1.jpg" ] =                                   heatx_heatx_j21_hs1;
    tempIconMap[ "2dicons/heatx/heatx.ecn_hs_2.jpg" ] =                                  heatx_heatx_ecn_hs_2;
    tempIconMap[ "2dicons/heatx/heatx.j21_ht1.jpg" ] =                                   heatx_heatx_j21_ht1;
    tempIconMap[ "2dicons/heatx/heatx.x_ht_1.jpg" ] =                                    heatx_heatx_x_ht_1;
    tempIconMap[ "2dicons/heatx/heatx.forced_2.jpg" ] =                                  heatx_heatx_forced_2;
    tempIconMap[ "2dicons/heatx/heatx.forced_1.jpg" ] =                                  heatx_heatx_forced_1;
    tempIconMap[ "2dicons/heatx/heatx.simp_hs.jpg" ] =                                   heatx_heatx_simp_hs;
    tempIconMap[ "2dicons/heatx/heatx.h_hs_2.jpg" ] =                                    heatx_heatx_h_hs_2;
    tempIconMap[ "2dicons/heatx/heatx.gen_hs.jpg" ] =                                    heatx_heatx_gen_hs;
    tempIconMap[ "2dicons/heatx/heatx.f_ht_4.jpg" ] =                                    heatx_heatx_f_ht_4;
    tempIconMap[ "2dicons/heatx/heatx.f_ht_2cn.jpg" ] =                                  heatx_heatx_f_ht_2cn;
    tempIconMap[ "2dicons/heatx/heatx.j21_hs2.jpg" ] =                                   heatx_heatx_j21_hs2;
    tempIconMap[ "2dicons/heatx/heatx.j12_ht1.jpg" ] =                                   heatx_heatx_j12_ht1;
    tempIconMap[ "2dicons/heatx/heatx.g_hs_2.jpg" ] =                                    heatx_heatx_g_hs_2;
    tempIconMap[ "2dicons/heatx/heatx.ecn_hs_1.jpg" ] =                                  heatx_heatx_ecn_hs_1;
    tempIconMap[ "2dicons/heatx/heatx.e_ht_2.jpg" ] =                                    heatx_heatx_e_ht_2;
    tempIconMap[ "2dicons/heatx/heatx.simp_ht.jpg" ] =                                   heatx_heatx_simp_ht;
    tempIconMap[ "2dicons/heatx/heatx.natura_1.jpg" ] =                                  heatx_heatx_natura_1;
    tempIconMap[ "2dicons/heatx/heatx.x_hs_2.jpg" ] =                                    heatx_heatx_x_hs_2;
    tempIconMap[ "2dicons/heatx/heatx.e_hs_1cn.jpg" ] =                                  heatx_heatx_e_hs_1cn;
    tempIconMap[ "2dicons/heatx/heatx.e_ht_1cn.jpg" ] =                                  heatx_heatx_e_ht_1cn;
    tempIconMap[ "2dicons/heatx/heatx.induce_2.jpg" ] =                                  heatx_heatx_induce_2;
    tempIconMap[ "2dicons/heatx/heatx.block.jpg" ] =                                     heatx_heatx_block;
    tempIconMap[ "2dicons/heatx/heatx.ecn_ht_2.jpg" ] =                                  heatx_heatx_ecn_ht_2;
    tempIconMap[ "2dicons/heatx/heatx.j12_hs1.jpg" ] =                                   heatx_heatx_j12_hs1;
    tempIconMap[ "2dicons/heatx/heatx.f_ht_2co.jpg" ] =                                  heatx_heatx_f_ht_2co;
    tempIconMap[ "2dicons/heatx/heatx.j21_ht2.jpg" ] =                                   heatx_heatx_j21_ht2;
    tempIconMap[ "2dicons/heatx/heatx.e_hs_1co.jpg" ] =                                  heatx_heatx_e_hs_1co;
    tempIconMap[ "2dicons/heatx/heatx.f_hs_2cn.jpg" ] =                                  heatx_heatx_f_hs_2cn;
    tempIconMap[ "2dicons/heatx/heatx.natura_2.jpg" ] =                                  heatx_heatx_natura_2;
    tempIconMap[ "2dicons/heatx/heatx.f_hs_4.jpg" ] =                                    heatx_heatx_f_hs_4;
    tempIconMap[ "2dicons/heatx/heatx.e_ht_1co.jpg" ] =                                  heatx_heatx_e_ht_1co;
    tempIconMap[ "2dicons/heatx/heatx.induce_1.jpg" ] =                                  heatx_heatx_induce_1;
    tempIconMap[ "2dicons/heatx/heatx.ecn_ht_1.jpg" ] =                                  heatx_heatx_ecn_ht_1;
    tempIconMap[ "2dicons/heatx/heatx.g_ht_2.jpg" ] =                                    heatx_heatx_g_ht_2;
    tempIconMap[ "2dicons/heatx/heatx.e_hs_2.jpg" ] =                                    heatx_heatx_e_hs_2;
    tempIconMap[ "2dicons/heatx/heatx.h_ht_2.jpg" ] =                                    heatx_heatx_h_ht_2;
    tempIconMap[ "2dicons/heatx/heatx.f_hs_2co.jpg" ] =                                  heatx_heatx_f_hs_2co;
    tempIconMap[ "2dicons/heatx/heatx.x_hs_1.jpg" ] =                                    heatx_heatx_x_hs_1;
    tempIconMap[ "2dicons/heatx/heatx.j12_ht2.jpg" ] =                                   heatx_heatx_j12_ht2;
    tempIconMap[ "2dicons/ccd/ccd.block.jpg" ] =                                         ccd_ccd_block;
    tempIconMap[ "2dicons/ccd/ccd.icon.jpg" ] =                                          ccd_ccd_icon;
    tempIconMap[ "2dicons/hierarchy/hierarchy.block.jpg" ] =                             hierarchy_hierarchy_block;
    tempIconMap[ "2dicons/compr/compr.icon1.jpg" ] =                                     compr_compr_icon1;
    tempIconMap[ "2dicons/compr/compr.block.jpg" ] =                                     compr_compr_block;
    tempIconMap[ "2dicons/compr/compr.icon3.jpg" ] =                                     compr_compr_icon3;
    tempIconMap[ "2dicons/compr/compr.icon2.jpg" ] =                                     compr_compr_icon2;
    tempIconMap[ "2dicons/ssplit/ssplit.triangle.jpg" ] =                                ssplit_ssplit_triangle;
    tempIconMap[ "2dicons/ssplit/ssplit.3way.jpg" ] =                                    ssplit_ssplit_3way;
    tempIconMap[ "2dicons/ssplit/ssplit.block.jpg" ] =                                   ssplit_ssplit_block;
    tempIconMap[ "2dicons/ssplit/ssplit.screen.jpg" ] =                                  ssplit_ssplit_screen;
    tempIconMap[ "2dicons/ssplit/ssplit.cyclone.jpg" ] =                                 ssplit_ssplit_cyclone;
    tempIconMap[ "2dicons/ssplit/ssplit.dot.jpg" ] =                                     ssplit_ssplit_dot;
    tempIconMap[ "2dicons/ssplit/ssplit.vscrub.jpg" ] =                                  ssplit_ssplit_vscrub;
    tempIconMap[ "2dicons/ssplit/ssplit.ccd.jpg" ] =                                     ssplit_ssplit_ccd;
    tempIconMap[ "2dicons/ssplit/ssplit.filter1.jpg" ] =                                 ssplit_ssplit_filter1;
    tempIconMap[ "2dicons/ssplit/ssplit.cfuge.jpg" ] =                                   ssplit_ssplit_cfuge;
    tempIconMap[ "2dicons/ssplit/ssplit.tee.jpg" ] =                                     ssplit_ssplit_tee;
    tempIconMap[ "2dicons/ssplit/ssplit.filter2.jpg" ] =                                 ssplit_ssplit_filter2;
    tempIconMap[ "2dicons/cyclone/cyclone.block.jpg" ] =                                 cyclone_cyclone_block;
    tempIconMap[ "2dicons/cyclone/cyclone.icon.jpg" ] =                                  cyclone_cyclone_icon;
    tempIconMap[ "2dicons/mheatx/mheatx.block2.jpg" ] =                                  mheatx_mheatx_block2;
    tempIconMap[ "2dicons/mheatx/mheatx.cocurnt.jpg" ] =                                 mheatx_mheatx_cocurnt;
    tempIconMap[ "2dicons/mheatx/mheatx.counter2.jpg" ] =                                mheatx_mheatx_counter2;
    tempIconMap[ "2dicons/mheatx/mheatx.block3.jpg" ] =                                  mheatx_mheatx_block3;
    tempIconMap[ "2dicons/mheatx/mheatx.block.jpg" ] =                                   mheatx_mheatx_block;
    tempIconMap[ "2dicons/mheatx/mheatx.simp_mhx.jpg" ] =                                mheatx_mheatx_simp_mhx;
    tempIconMap[ "2dicons/mheatx/mheatx.icon1.jpg" ] =                                   mheatx_mheatx_icon1;
    tempIconMap[ "2dicons/mheatx/mheatx.counter.jpg" ] =                                 mheatx_mheatx_counter;
    tempIconMap[ "2dicons/mheatx/mheatx.circ_mhx.jpg" ] =                                mheatx_mheatx_circ_mhx;
    tempIconMap[ "2dicons/fsplit/fsplit.triangle.jpg" ] =                                fsplit_fsplit_triangle;
    tempIconMap[ "2dicons/fsplit/fsplit.dot.jpg" ] =                                     fsplit_fsplit_dot;
    tempIconMap[ "2dicons/fsplit/fsplit.3way.jpg" ] =                                    fsplit_fsplit_3way;
    tempIconMap[ "2dicons/fsplit/fsplit.heat_tee.jpg" ] =                                fsplit_fsplit_heat_tee;
    tempIconMap[ "2dicons/fsplit/fsplit.block.jpg" ] =                                   fsplit_fsplit_block;
    tempIconMap[ "2dicons/fsplit/fsplit.work.jpg" ] =                                    fsplit_fsplit_work;
    tempIconMap[ "2dicons/fsplit/fsplit.tee.jpg" ] =                                     fsplit_fsplit_tee;
    tempIconMap[ "2dicons/fsplit/fsplit.heat.jpg" ] =                                    fsplit_fsplit_heat;
    tempIconMap[ "2dicons/fsplit/fsplit.work_tee.jpg" ] =                                fsplit_fsplit_work_tee;
    tempIconMap[ "2dicons/swash/swash.icon.jpg" ] =                                      swash_swash_icon;
    tempIconMap[ "2dicons/swash/swash.block.jpg" ] =                                     swash_swash_block;
    tempIconMap[ "2dicons/radfrac/radfrac.absbr2.jpg" ] =                                radfrac_radfrac_absbr2;
    tempIconMap[ "2dicons/radfrac/radfrac.packcol1.jpg" ] =                              radfrac_radfrac_packcol1;
    tempIconMap[ "2dicons/radfrac/radfrac.fract2.jpg" ] =                                radfrac_radfrac_fract2;
    tempIconMap[ "2dicons/radfrac/radfrac.strip1.jpg" ] =                                radfrac_radfrac_strip1;
    tempIconMap[ "2dicons/radfrac/radfrac.decant1.jpg" ] =                               radfrac_radfrac_decant1;
    tempIconMap[ "2dicons/radfrac/radfrac.rect.jpg" ] =                                  radfrac_radfrac_rect;
    tempIconMap[ "2dicons/radfrac/radfrac.absbr1.jpg" ] =                                radfrac_radfrac_absbr1;
    tempIconMap[ "2dicons/radfrac/radfrac.strip2.jpg" ] =                                radfrac_radfrac_strip2;
    tempIconMap[ "2dicons/radfrac/radfrac.packstr1.jpg" ] =                              radfrac_radfrac_packstr1;
    tempIconMap[ "2dicons/radfrac/radfrac.block.jpg" ] =                                 radfrac_radfrac_block;
    tempIconMap[ "2dicons/radfrac/radfrac.packstr2.jpg" ] =                              radfrac_radfrac_packstr2;
    tempIconMap[ "2dicons/radfrac/radfrac.absbr3.jpg" ] =                                radfrac_radfrac_absbr3;
    tempIconMap[ "2dicons/radfrac/radfrac.packcol2.jpg" ] =                              radfrac_radfrac_packcol2;
    tempIconMap[ "2dicons/radfrac/radfrac.decant3.jpg" ] =                               radfrac_radfrac_decant3;
    tempIconMap[ "2dicons/radfrac/radfrac.packabs.jpg" ] =                               radfrac_radfrac_packabs;
    tempIconMap[ "2dicons/radfrac/radfrac.decant2.jpg" ] =                               radfrac_radfrac_decant2;
    tempIconMap[ "2dicons/radfrac/radfrac.fract1.jpg" ] =                                radfrac_radfrac_fract1;
    tempIconMap[ "2dicons/analyzer/analyzer.block.jpg" ] =                               analyzer_analyzer_block;
    tempIconMap[ "2dicons/analyzer/analyzer.analyzer.jpg" ] =                            analyzer_analyzer_analyzer;
    tempIconMap[ "2dicons/analyzer/analyzer.analyze2.jpg" ] =                            analyzer_analyzer_analyze2;
    tempIconMap[ "2dicons/heater/heater.valve.jpg" ] =                                   heater_heater_valve;
    tempIconMap[ "2dicons/heater/heater.furnace.jpg" ] =                                 heater_heater_furnace;
    tempIconMap[ "2dicons/heater/heater.valve2.jpg" ] =                                  heater_heater_valve2;
    tempIconMap[ "2dicons/heater/heater.heater.jpg" ] =                                  heater_heater_heater;
    tempIconMap[ "2dicons/heater/heater.aircooler.jpg" ] =                               heater_heater_aircooler;
    tempIconMap[ "2dicons/heater/heater.block.jpg" ] =                                   heater_heater_block;
    tempIconMap[ "2dicons/heater/heater.compr.jpg" ] =                                   heater_heater_compr;
    tempIconMap[ "2dicons/heater/heater.pump.jpg" ] =                                    heater_heater_pump;
    tempIconMap[ "2dicons/heater/heater.valve4.jpg" ] =                                  heater_heater_valve4;
    tempIconMap[ "2dicons/rgibbs/rgibbs.icon1.jpg" ] =                                   rgibbs_rgibbs_icon1;
    tempIconMap[ "2dicons/rgibbs/rgibbs.icon2.jpg" ] =                                   rgibbs_rgibbs_icon2;
    tempIconMap[ "2dicons/rgibbs/rgibbs.block.jpg" ] =                                   rgibbs_rgibbs_block;
    tempIconMap[ "2dicons/hetran/hetran.e_ht_2.jpg" ] =                                  hetran_hetran_e_ht_2;
    tempIconMap[ "2dicons/hetran/hetran.e_hs_1co.jpg" ] =                                hetran_hetran_e_hs_1co;
    tempIconMap[ "2dicons/hetran/hetran.f_ht_2cn.jpg" ] =                                hetran_hetran_f_ht_2cn;
    tempIconMap[ "2dicons/hetran/hetran.g_ht_2.jpg" ] =                                  hetran_hetran_g_ht_2;
    tempIconMap[ "2dicons/hetran/hetran.smp_ht.jpg" ] =                                  hetran_hetran_smp_ht;
    tempIconMap[ "2dicons/hetran/hetran.f_ht_2co.jpg" ] =                                hetran_hetran_f_ht_2co;
    tempIconMap[ "2dicons/hetran/hetran.e_hs_2.jpg" ] =                                  hetran_hetran_e_hs_2;
    tempIconMap[ "2dicons/hetran/hetran.j21_hs1.jpg" ] =                                 hetran_hetran_j21_hs1;
    tempIconMap[ "2dicons/hetran/hetran.j12_ht1.jpg" ] =                                 hetran_hetran_j12_ht1;
    tempIconMap[ "2dicons/hetran/hetran.k_ht_1.jpg" ] =                                  hetran_hetran_k_ht_1;
    tempIconMap[ "2dicons/hetran/hetran.j12_hs1.jpg" ] =                                 hetran_hetran_j12_hs1;
    tempIconMap[ "2dicons/hetran/hetran.j12_hs2.jpg" ] =                                 hetran_hetran_j12_hs2;
    tempIconMap[ "2dicons/hetran/hetran.j12_ht2.jpg" ] =                                 hetran_hetran_j12_ht2;
    tempIconMap[ "2dicons/hetran/hetran.j21_ht1.jpg" ] =                                 hetran_hetran_j21_ht1;
    tempIconMap[ "2dicons/hetran/hetran.x_hs_2.jpg" ] =                                  hetran_hetran_x_hs_2;
    tempIconMap[ "2dicons/hetran/hetran.e_ht_1cn.jpg" ] =                                hetran_hetran_e_ht_1cn;
    tempIconMap[ "2dicons/hetran/hetran.gen_hs.jpg" ] =                                  hetran_hetran_gen_hs;
    tempIconMap[ "2dicons/hetran/hetran.g_hs_2.jpg" ] =                                  hetran_hetran_g_hs_2;
    tempIconMap[ "2dicons/hetran/hetran.j21_ht2.jpg" ] =                                 hetran_hetran_j21_ht2;
    tempIconMap[ "2dicons/hetran/hetran.h_hs_2.jpg" ] =                                  hetran_hetran_h_hs_2;
    tempIconMap[ "2dicons/hetran/hetran.gen_ht.jpg" ] =                                  hetran_hetran_gen_ht;
    tempIconMap[ "2dicons/hetran/hetran.f_hs_4.jpg" ] =                                  hetran_hetran_f_hs_4;
    tempIconMap[ "2dicons/hetran/hetran.x_ht_1.jpg" ] =                                  hetran_hetran_x_ht_1;
    tempIconMap[ "2dicons/hetran/hetran.f_hs_2co.jpg" ] =                                hetran_hetran_f_hs_2co;
    tempIconMap[ "2dicons/hetran/hetran.block.jpg" ] =                                   hetran_hetran_block;
    tempIconMap[ "2dicons/hetran/hetran.j21_hs2.jpg" ] =                                 hetran_hetran_j21_hs2;
    tempIconMap[ "2dicons/hetran/hetran.f_hs_2cn.jpg" ] =                                hetran_hetran_f_hs_2cn;
    tempIconMap[ "2dicons/hetran/hetran.h_ht_2.jpg" ] =                                  hetran_hetran_h_ht_2;
    tempIconMap[ "2dicons/hetran/hetran.e_ht_1co.jpg" ] =                                hetran_hetran_e_ht_1co;
    tempIconMap[ "2dicons/hetran/hetran.simp_hs.jpg" ] =                                 hetran_hetran_simp_hs;
    tempIconMap[ "2dicons/hetran/hetran.k_ht_2.jpg" ] =                                  hetran_hetran_k_ht_2;
    tempIconMap[ "2dicons/hetran/hetran.e_hs_1cn.jpg" ] =                                hetran_hetran_e_hs_1cn;
    tempIconMap[ "2dicons/hetran/hetran.x_hs_1.jpg" ] =                                  hetran_hetran_x_hs_1;
    tempIconMap[ "2dicons/hetran/hetran.x_ht_2.jpg" ] =                                  hetran_hetran_x_ht_2;
    tempIconMap[ "2dicons/hetran/hetran.f_ht_4.jpg" ] =                                  hetran_hetran_f_ht_4;
    tempIconMap[ "2dicons/aerotran/aerotran.ecn_hs_1.jpg" ] =                            aerotran_aerotran_ecn_hs_1;
    tempIconMap[ "2dicons/aerotran/aerotran.forced_2.jpg" ] =                            aerotran_aerotran_forced_2;
    tempIconMap[ "2dicons/aerotran/aerotran.natura_2.jpg" ] =                            aerotran_aerotran_natura_2;
    tempIconMap[ "2dicons/aerotran/aerotran.induce_1.jpg" ] =                            aerotran_aerotran_induce_1;
    tempIconMap[ "2dicons/aerotran/aerotran.ecn_hs_2.jpg" ] =                            aerotran_aerotran_ecn_hs_2;
    tempIconMap[ "2dicons/aerotran/aerotran.natura_1.jpg" ] =                            aerotran_aerotran_natura_1;
    tempIconMap[ "2dicons/aerotran/aerotran.ecn_ht_1.jpg" ] =                            aerotran_aerotran_ecn_ht_1;
    tempIconMap[ "2dicons/aerotran/aerotran.induce_2.jpg" ] =                            aerotran_aerotran_induce_2;
    tempIconMap[ "2dicons/aerotran/aerotran.ecn_ht_2.jpg" ] =                            aerotran_aerotran_ecn_ht_2;
    tempIconMap[ "2dicons/aerotran/aerotran.block.jpg" ] =                               aerotran_aerotran_block;
    tempIconMap[ "2dicons/aerotran/aerotran.forced_1.jpg" ] =                            aerotran_aerotran_forced_1;
    tempIconMap[ "2dicons/mult/mult.work.jpg" ] =                                        mult_mult_work;
    tempIconMap[ "2dicons/mult/mult.block.jpg" ] =                                       mult_mult_block;
    tempIconMap[ "2dicons/mult/mult.heat.jpg" ] =                                        mult_mult_heat;
    tempIconMap[ "2dicons/mult/mult.dot.jpg" ] =                                         mult_mult_dot;
    tempIconMap[ "2dicons/batchfrac/batchfrac.column3.jpg" ] =                           batchfrac_batchfrac_column3;
    tempIconMap[ "2dicons/batchfrac/batchfrac.column2.jpg" ] =                           batchfrac_batchfrac_column2;
    tempIconMap[ "2dicons/batchfrac/batchfrac.column1.jpg" ] =                           batchfrac_batchfrac_column1;
    tempIconMap[ "2dicons/batchfrac/batchfrac.block.jpg" ] =                             batchfrac_batchfrac_block;
    tempIconMap[ "2dicons/scfrac/scfrac.block.jpg" ] =                                   scfrac_scfrac_block;
    tempIconMap[ "2dicons/scfrac/scfrac.cdu2.jpg" ] =                                    scfrac_scfrac_cdu2;
    tempIconMap[ "2dicons/scfrac/scfrac.vacuum2.jpg" ] =                                 scfrac_scfrac_vacuum2;
    tempIconMap[ "2dicons/scfrac/scfrac.cdu3.jpg" ] =                                    scfrac_scfrac_cdu3;
    tempIconMap[ "2dicons/scfrac/scfrac.cdu1.jpg" ] =                                    scfrac_scfrac_cdu1;
    tempIconMap[ "2dicons/scfrac/scfrac.vacuum1.jpg" ] =                                 scfrac_scfrac_vacuum1;
    tempIconMap[ "2dicons/mcompr/mcompr.icon1.jpg" ] =                                   mcompr_mcompr_icon1;
    tempIconMap[ "2dicons/mcompr/mcompr.block.jpg" ] =                                   mcompr_mcompr_block;
    tempIconMap[ "2dicons/vscrub/vscrub.icon.jpg" ] =                                    vscrub_vscrub_icon;
    tempIconMap[ "2dicons/vscrub/vscrub.block.jpg" ] =                                   vscrub_vscrub_block;
    tempIconMap[ "2dicons/flash2/flash2.heater.jpg" ] =                                  flash2_flash2_heater;
    tempIconMap[ "2dicons/flash2/flash2.v_drum1.jpg" ] =                                 flash2_flash2_v_drum1;
    tempIconMap[ "2dicons/flash2/flash2.block.jpg" ] =                                   flash2_flash2_block;
    tempIconMap[ "2dicons/flash2/flash2.furnace.jpg" ] =                                 flash2_flash2_furnace;
    tempIconMap[ "2dicons/flash2/flash2.h_drum.jpg" ] =                                  flash2_flash2_h_drum;
    tempIconMap[ "2dicons/flash2/flash2.v_drum2.jpg" ] =                                 flash2_flash2_v_drum2;
    tempIconMap[ "2dicons/selector/selector.work.jpg" ] =                                selector_selector_work;
    tempIconMap[ "2dicons/selector/selector.heat.jpg" ] =                                selector_selector_heat;
    tempIconMap[ "2dicons/selector/selector.triangle.jpg" ] =                            selector_selector_triangle;
    tempIconMap[ "2dicons/selector/selector.block.jpg" ] =                               selector_selector_block;
    tempIconMap[ "2dicons/sep/sep.block.jpg" ] =                                         sep_sep_block;
    tempIconMap[ "2dicons/sep/sep.icon1.jpg" ] =                                         sep_sep_icon1;
    tempIconMap[ "2dicons/sep/sep.icon3.jpg" ] =                                         sep_sep_icon3;
    tempIconMap[ "2dicons/sep/sep.icon2.jpg" ] =                                         sep_sep_icon2;
    tempIconMap[ "2dicons/dryer/dryer.fluidbed2.jpg" ] =                                 dryer_dryer_fluidbed2;
    tempIconMap[ "2dicons/dryer/dryer.spray.jpg" ] =                                     dryer_dryer_spray;
    tempIconMap[ "2dicons/dryer/dryer.fluidbed.jpg" ] =                                  dryer_dryer_fluidbed;
    tempIconMap[ "2dicons/dryer/dryer.block.jpg" ] =                                     dryer_dryer_block;
    tempIconMap[ "2dicons/filter/filter.rotary.jpg" ] =                                  filter_filter_rotary;
    tempIconMap[ "2dicons/filter/filter.plate.jpg" ] =                                   filter_filter_plate;
    tempIconMap[ "2dicons/filter/filter.block.jpg" ] =                                   filter_filter_block;
    tempIconMap[ "2dicons/crusher/crusher.icon.jpg" ] =                                  crusher_crusher_icon;
    tempIconMap[ "2dicons/crusher/crusher.block.jpg" ] =                                 crusher_crusher_block;
    tempIconMap[ "2dicons/user/user.block.jpg" ] =                                       user_user_block;
    tempIconMap[ "2dicons/user/user.small.jpg" ] =                                       user_user_small;
    tempIconMap[ "2dicons/dupl/dupl.work.jpg" ] =                                        dupl_dupl_work;
    tempIconMap[ "2dicons/dupl/dupl.heat.jpg" ] =                                        dupl_dupl_heat;
    tempIconMap[ "2dicons/dupl/dupl.block.jpg" ] =                                       dupl_dupl_block;
    tempIconMap[ "2dicons/dupl/dupl.dot.jpg" ] =                                         dupl_dupl_dot;
    tempIconMap[ "2dicons/rbatch/rbatch.icon1.jpg" ] =                                   rbatch_rbatch_icon1;
    tempIconMap[ "2dicons/rbatch/rbatch.block.jpg" ] =                                   rbatch_rbatch_block;
    tempIconMap[ "2dicons/distl/distl.block.jpg" ] =                                     distl_distl_block;
    tempIconMap[ "2dicons/distl/distl.icon1.jpg" ] =                                     distl_distl_icon1;
    tempIconMap[ "2dicons/distl/distl.icon2.jpg" ] =                                     distl_distl_icon2;
    tempIconMap[ "2dicons/crystallizer/crystallizer.block.jpg" ] =                       crystallizer_crystallizer_block;
    tempIconMap[ "2dicons/crystallizer/crystallizer.icon1.jpg" ] =                       crystallizer_crystallizer_icon1;
    tempIconMap[ "2dicons/crystallizer/crystallizer.icon2.jpg" ] =                       crystallizer_crystallizer_icon2;
    tempIconMap[ "2dicons/screen/screen.icon2.jpg" ] =                                   screen_screen_icon2;
    tempIconMap[ "2dicons/screen/screen.block.jpg" ] =                                   screen_screen_block;
    tempIconMap[ "2dicons/screen/screen.icon1.jpg" ] =                                   screen_screen_icon1;
    tempIconMap[ "2dicons/rcstr/rcstr.icon1.jpg" ] =                                     rcstr_rcstr_icon1;
    tempIconMap[ "2dicons/rcstr/rcstr.block.jpg" ] =                                     rcstr_rcstr_block;
    tempIconMap[ "2dicons/ryield/ryield.block.jpg" ] =                                   ryield_ryield_block;
    tempIconMap[ "2dicons/ryield/ryield.icon2.jpg" ] =                                   ryield_ryield_icon2;
    tempIconMap[ "2dicons/ryield/ryield.icon3.jpg" ] =                                   ryield_ryield_icon3;
    tempIconMap[ "2dicons/sep2/sep2.icon1.jpg" ] =                                       sep2_sep2_icon1;
    tempIconMap[ "2dicons/sep2/sep2.icon2.jpg" ] =                                       sep2_sep2_icon2;
    tempIconMap[ "2dicons/sep2/sep2.block.jpg" ] =                                       sep2_sep2_block;
    tempIconMap[ "2dicons/sep2/sep2.icon3.jpg" ] =                                       sep2_sep2_icon3;
    tempIconMap[ "2dicons/fabfl/fabfl.icon.jpg" ] =                                      fabfl_fabfl_icon;
    tempIconMap[ "2dicons/fabfl/fabfl.block.jpg" ] =                                     fabfl_fabfl_block;
    tempIconMap[ "2dicons/clchng/clchng.icon1.jpg" ] =                                   clchng_clchng_icon1;
    tempIconMap[ "2dicons/clchng/clchng.block.jpg" ] =                                   clchng_clchng_block;
    tempIconMap[ "2dicons/qtvec/qtvec.block.jpg" ] =                                     qtvec_qtvec_block;
    tempIconMap[ "2dicons/qtvec/qtvec.dot.jpg" ] =                                       qtvec_qtvec_dot;
    tempIconMap[ "2dicons/hycyc/hycyc.icon.jpg" ] =                                      hycyc_hycyc_icon;
    tempIconMap[ "2dicons/hycyc/hycyc.block.jpg" ] =                                     hycyc_hycyc_block;
    tempIconMap[ "2dicons/mixer/mixer.valve.jpg" ] =                                     mixer_mixer_valve;
    tempIconMap[ "2dicons/mixer/mixer.tee.jpg" ] =                                       mixer_mixer_tee;
    tempIconMap[ "2dicons/mixer/mixer.heat.jpg" ] =                                      mixer_mixer_heat;
    tempIconMap[ "2dicons/mixer/mixer.screw.jpg" ] =                                     mixer_mixer_screw;
    tempIconMap[ "2dicons/mixer/mixer.triangle.jpg" ] =                                  mixer_mixer_triangle;
    tempIconMap[ "2dicons/mixer/mixer.hopper.jpg" ] =                                    mixer_mixer_hopper;
    tempIconMap[ "2dicons/mixer/mixer.3way.jpg" ] =                                      mixer_mixer_3way;
    tempIconMap[ "2dicons/mixer/mixer.dot.jpg" ] =                                       mixer_mixer_dot;
    tempIconMap[ "2dicons/mixer/mixer.work.jpg" ] =                                      mixer_mixer_work;
    tempIconMap[ "2dicons/mixer/mixer.block.jpg" ] =                                     mixer_mixer_block;
    tempIconMap[ "2dicons/mixer/mixer.tank.jpg" ] =                                      mixer_mixer_tank;
    tempIconMap[ "2dicons/pipe/pipe.u_pipe.jpg" ] =                                      pipe_pipe_u_pipe;
    tempIconMap[ "2dicons/pipe/pipe.d_pipe.jpg" ] =                                      pipe_pipe_d_pipe;
    tempIconMap[ "2dicons/pipe/pipe.block.jpg" ] =                                       pipe_pipe_block;
    tempIconMap[ "2dicons/pipe/pipe.hi_pipe.jpg" ] =                                     pipe_pipe_hi_pipe;
    tempIconMap[ "2dicons/pipe/pipe.v_pipe.jpg" ] =                                      pipe_pipe_v_pipe;
    tempIconMap[ "2dicons/extract/extract.icon2.jpg" ] =                                 extract_extract_icon2;
    tempIconMap[ "2dicons/extract/extract.pod.jpg" ] =                                   extract_extract_pod;
    tempIconMap[ "2dicons/extract/extract.icon1.jpg" ] =                                 extract_extract_icon1;
    tempIconMap[ "2dicons/extract/extract.block.jpg" ] =                                 extract_extract_block;
    tempIconMap[ "2dicons/pipeline/pipeline.block.jpg" ] =                               pipeline_pipeline_block;
    tempIconMap[ "2dicons/pipeline/pipeline.hi_pipe.jpg" ] =                             pipeline_pipeline_hi_pipe;
    tempIconMap[ "2dicons/pipeline/pipeline.u_pipe.jpg" ] =                              pipeline_pipeline_u_pipe;
    tempIconMap[ "2dicons/pipeline/pipeline.d_pipe.jpg" ] =                              pipeline_pipeline_d_pipe;
    tempIconMap[ "2dicons/pipeline/pipeline.v_pipe.jpg" ] =                              pipeline_pipeline_v_pipe;
    tempIconMap[ "2dicons/user2/user2.strip.jpg" ] =                                     user2_user2_strip;
    tempIconMap[ "2dicons/user2/user2.cstr.jpg" ] =                                      user2_user2_cstr;
    tempIconMap[ "2dicons/user2/user2.h_drum.jpg" ] =                                    user2_user2_h_drum;
    tempIconMap[ "2dicons/user2/user2.heater.jpg" ] =                                    user2_user2_heater;
    tempIconMap[ "2dicons/user2/user2.excel.jpg" ] =                                     user2_user2_excel;
    tempIconMap[ "2dicons/user2/user2.valve4.jpg" ] =                                    user2_user2_valve4;
    tempIconMap[ "2dicons/user2/user2.v_drum.jpg" ] =                                    user2_user2_v_drum;
    tempIconMap[ "2dicons/user2/user2.filter.jpg" ] =                                    user2_user2_filter;
    tempIconMap[ "2dicons/user2/user2.heatx2.jpg" ] =                                    user2_user2_heatx2;
    tempIconMap[ "2dicons/user2/user2.heatx1.jpg" ] =                                    user2_user2_heatx1;
    tempIconMap[ "2dicons/user2/user2.cfuge.jpg" ] =                                     user2_user2_cfuge;
    tempIconMap[ "2dicons/user2/user2.block.jpg" ] =                                     user2_user2_block;
    tempIconMap[ "2dicons/user2/user2.reactor.jpg" ] =                                   user2_user2_reactor;
    tempIconMap[ "2dicons/user2/user2.plug.jpg" ] =                                      user2_user2_plug;
    tempIconMap[ "2dicons/user2/user2.rect.jpg" ] =                                      user2_user2_rect;
    tempIconMap[ "2dicons/user2/user2.fract.jpg" ] =                                     user2_user2_fract;
    tempIconMap[ "2dicons/valve/valve.block.jpg" ] =                                     valve_valve_block;
    tempIconMap[ "2dicons/valve/valve.valve3.jpg" ] =                                    valve_valve_valve3;
    tempIconMap[ "2dicons/valve/valve.valve1.jpg" ] =                                    valve_valve_valve1;
    tempIconMap[ "2dicons/valve/valve.valve2.jpg" ] =                                    valve_valve_valve2;
    tempIconMap[ "2dicons/valve/valve.valve4.jpg" ] =                                    valve_valve_valve4;
    tempIconMap[ "2dicons/rplug/rplug.icon2.jpg" ] =                                     rplug_rplug_icon2;
    tempIconMap[ "2dicons/rplug/rplug.icon3.jpg" ] =                                     rplug_rplug_icon3;
    tempIconMap[ "2dicons/rplug/rplug.icon1.jpg" ] =                                     rplug_rplug_icon1;
    tempIconMap[ "2dicons/rplug/rplug.block.jpg" ] =                                     rplug_rplug_block;
    tempIconMap[ "2dicons/pump/pump.icon1.jpg" ] =                                       pump_pump_icon1;
    tempIconMap[ "2dicons/pump/pump.icon2.jpg" ] =                                       pump_pump_icon2;
    tempIconMap[ "2dicons/pump/pump.block.jpg" ] =                                       pump_pump_block;
    tempIconMap[ "2dicons/flash3/flash3.v_drum2.jpg" ] =                                 flash3_flash3_v_drum2;
    tempIconMap[ "2dicons/flash3/flash3.heater.jpg" ] =                                  flash3_flash3_heater;
    tempIconMap[ "2dicons/flash3/flash3.h_drum.jpg" ] =                                  flash3_flash3_h_drum;
    tempIconMap[ "2dicons/flash3/flash3.block.jpg" ] =                                   flash3_flash3_block;
    tempIconMap[ "2dicons/flash3/flash3.v_drum1.jpg" ] =                                 flash3_flash3_v_drum1;
    tempIconMap[ "2dicons/flash3/flash3.furnace.jpg" ] =                                 flash3_flash3_furnace;
    tempIconMap[ "2dicons/pipe2/pipe2_pipe2_contract.png" ] =                            pipe2_pipe2_contract;
    tempIconMap[ "2dicons/pipe2/pipe2_pipe2_u_pipe.png" ] =                              pipe2_pipe2_u_pipe;
    tempIconMap[ "2dicons/pipe2/pipe2_pipe2_expand.png" ] =                              pipe2_pipe2_expand;
    tempIconMap[ "2dicons/pipe2/pipe2_pipe2_h_pipe.png" ] =                              pipe2_pipe2_h_pipe;
    tempIconMap[ "2dicons/pipe2/pipe2_pipe2_block.png" ] =                               pipe2_pipe2_block;
    tempIconMap[ "2dicons/pipe2/pipe2_pipe2_d_pipe.png" ] =                              pipe2_pipe2_d_pipe;
    tempIconMap[ "2dicons/pipe2/pipe2_pipe2_b_pipe.png" ] =                              pipe2_pipe2_b_pipe;
    tempIconMap[ "2dicons/pipe2/pipe2_pipe2_v_pipe.png" ] =                              pipe2_pipe2_v_pipe;
    tempIconMap[ "2dicons/fsplit_work/fsplit_work_fsplit_work_work_tee.png" ] =          fsplit_work_fsplit_work_work_tee;
    tempIconMap[ "2dicons/fsplit_work/fsplit_work_fsplit_work_triangle.png" ] =          fsplit_work_fsplit_work_triangle;
    tempIconMap[ "2dicons/fsplit_work/fsplit_work_fsplit_work_work.png" ] =              fsplit_work_fsplit_work_work;
    tempIconMap[ "2dicons/transform/transform_transform_icon1.png" ] =                   transform_transform_icon1;
    tempIconMap[ "2dicons/rplugpde/rplugpde_rplugpde_icon1.png" ] =                      rplugpde_rplugpde_icon1;
    tempIconMap[ "2dicons/rplugpde/rplugpde_rplugpde_icon3.png" ] =                      rplugpde_rplugpde_icon3;
    tempIconMap[ "2dicons/rplugpde/rplugpde_rplugpde_icon2.png" ] =                      rplugpde_rplugpde_icon2;
    tempIconMap[ "2dicons/rplugpde/rplugpde_rplugpde_block.png" ] =                      rplugpde_rplugpde_block;
    tempIconMap[ "2dicons/lag_1/lag_1_lag_1_icon1.png" ] =                               lag_1_lag_1_icon1;
    tempIconMap[ "2dicons/logicgate/logicgate_logicgate_logicgate.png" ] =               logicgate_logicgate_logicgate;
    tempIconMap[ "2dicons/mixer_work/mixer_work_mixer_work_work.png" ] =                 mixer_work_mixer_work_work;
    tempIconMap[ "2dicons/kineticsest/kineticsest_kineticsest_icon1.png" ] =             kineticsest_kineticsest_icon1;
    tempIconMap[ "2dicons/feedforward/feedforward_feedforward_icon1.png" ] =             feedforward_feedforward_icon1;
    tempIconMap[ "2dicons/mult_heat/mult_heat_mult_heat_heat.png" ] =                    mult_heat_mult_heat_heat;
    tempIconMap[ "2dicons/rcstr2/rcstr2_rcstr2_sperical.png" ] =                         rcstr2_rcstr2_sperical;
    tempIconMap[ "2dicons/rcstr2/rcstr2_rcstr2_block.png" ] =                            rcstr2_rcstr2_block;
    tempIconMap[ "2dicons/rcstr2/rcstr2_rcstr2_horizontal.png" ] =                       rcstr2_rcstr2_horizontal;
    tempIconMap[ "2dicons/rcstr2/rcstr2_rcstr2_vertical.png" ] =                         rcstr2_rcstr2_vertical;
    tempIconMap[ "2dicons/rcstr2/rcstr2_rcstr2_icon1.png" ] =                            rcstr2_rcstr2_icon1;
    tempIconMap[ "2dicons/onoffcontrol/onoffcontrol_onoffcontrol_digitalpoint.png" ] =   onoffcontrol_onoffcontrol_digitalpoint;
    tempIconMap[ "2dicons/dupl_heat/dupl_heat_dupl_heat_heat.png" ] =                    dupl_heat_dupl_heat_heat;
    tempIconMap[ "2dicons/dead_time/dead_time_dead_time_icon1.png" ] =                   dead_time_dead_time_icon1;
    tempIconMap[ "2dicons/rgibbs2/rgibbs2_rgibbs2_icon2.png" ] =                         rgibbs2_rgibbs2_icon2;
    tempIconMap[ "2dicons/rgibbs2/rgibbs2_rgibbs2_icon1.png" ] =                         rgibbs2_rgibbs2_icon1;
    tempIconMap[ "2dicons/rgibbs2/rgibbs2_rgibbs2_block.png" ] =                         rgibbs2_rgibbs2_block;
    tempIconMap[ "2dicons/multiply/multiply_multiply_icon1.png" ] =                      multiply_multiply_icon1;
    tempIconMap[ "2dicons/timedata/timedata_timedata_timedata.png" ] =                   timedata_timedata_timedata;
    tempIconMap[ "2dicons/expansion/expansion_expansion_expand1.png" ] =                 expansion_expansion_expand1;
    tempIconMap[ "2dicons/expansion/expansion_expansion_contract1.png" ] =               expansion_expansion_contract1;
    tempIconMap[ "2dicons/splitrange/splitrange_splitrange_icon1.png" ] =                splitrange_splitrange_icon1;
    tempIconMap[ "2dicons/polyfrac/polyfrac_polyfrac_h_drum.png" ] =                     polyfrac_polyfrac_h_drum;
    tempIconMap[ "2dicons/polyfrac/polyfrac_polyfrac_block.png" ] =                      polyfrac_polyfrac_block;
    tempIconMap[ "2dicons/polyfrac/polyfrac_polyfrac_heater.png" ] =                     polyfrac_polyfrac_heater;
    tempIconMap[ "2dicons/polyfrac/polyfrac_polyfrac_furnace.png" ] =                    polyfrac_polyfrac_furnace;
    tempIconMap[ "2dicons/polyfrac/polyfrac_polyfrac_v_drum2.png" ] =                    polyfrac_polyfrac_v_drum2;
    tempIconMap[ "2dicons/polyfrac/polyfrac_polyfrac_v_drum1.png" ] =                    polyfrac_polyfrac_v_drum1;
    tempIconMap[ "2dicons/scale/scale_scale_icon1.png" ] =                               scale_scale_icon1;
    tempIconMap[ "2dicons/dmcplus/dmcplus_dmcplus_icon1.png" ] =                         dmcplus_dmcplus_icon1;
    tempIconMap[ "2dicons/dmcplus/dmcplus_dmcplus_icon2.png" ] =                         dmcplus_dmcplus_icon2;
    tempIconMap[ "2dicons/multisum/multisum_multisum_icon1.png" ] =                      multisum_multisum_icon1;
    tempIconMap[ "2dicons/hiloselect/hiloselect_hiloselect_icon1.png" ] =                hiloselect_hiloselect_icon1;
    tempIconMap[ "2dicons/fsplit_heat/fsplit_heat_fsplit_heat_triangle.png" ] =          fsplit_heat_fsplit_heat_triangle;
    tempIconMap[ "2dicons/fsplit_heat/fsplit_heat_fsplit_heat_heat_tee.png" ] =          fsplit_heat_fsplit_heat_heat_tee;
    tempIconMap[ "2dicons/fsplit_heat/fsplit_heat_fsplit_heat_heat.png" ] =              fsplit_heat_fsplit_heat_heat;
    tempIconMap[ "2dicons/prbs/prbs_prbs_icon1.png" ] =                                  prbs_prbs_icon1;
    tempIconMap[ "2dicons/comparator/comparator_comparator_icon1.png" ] =                comparator_comparator_icon1;
    tempIconMap[ "2dicons/custom/apecs.png" ] =                                          apecs;
    tempIconMap[ "2dicons/custom/aspen.png" ] =                                          aspen;
    tempIconMap[ "2dicons/custom/dynamics.png" ] =                                       dynamics;
    tempIconMap[ "2dicons/mult_work/mult_work_mult_work_work.png" ] =                    mult_work_mult_work_work;
    tempIconMap[ "2dicons/multihiloselect/multihiloselect_multihiloselect_icon1.png" ] = multihiloselect_multihiloselect_icon1;
    tempIconMap[ "2dicons/pid/pid_pid_icon1.png" ] =                                     pid_pid_icon1;
    tempIconMap[ "2dicons/aplusproduct/aplusproduct_aplusproduct_arrow.png" ] =          aplusproduct_aplusproduct_arrow;
    tempIconMap[ "2dicons/psv/psv_psv_pfd1.png" ] =                                      psv_psv_pfd1;
    tempIconMap[ "2dicons/steamptot/steamptot_steamptot_icon1.png" ] =                   steamptot_steamptot_icon1;
    tempIconMap[ "2dicons/flash2/flash2_flash2_vertical.png" ] =                         flash2_flash2_vertical;
    tempIconMap[ "2dicons/flash2/flash2_flash2_icon1.png" ] =                            flash2_flash2_icon1;
    tempIconMap[ "2dicons/flash2/flash2_flash2_horizontal.png" ] =                       flash2_flash2_horizontal;
    tempIconMap[ "2dicons/flash2/flash2_flash2_sperical.png" ] =                         flash2_flash2_sperical;
    tempIconMap[ "2dicons/noise/noise_noise_icon1.png" ] =                               noise_noise_icon1;
    tempIconMap[ "2dicons/logiccompare/logiccompare_logiccompare_logiccompare.png" ] =   logiccompare_logiccompare_logiccompare;
    tempIconMap[ "2dicons/selector/selector_selector_dot.png" ] =                        selector_selector_dot;
    tempIconMap[ "2dicons/psv2/psv2_psv2_pfd1.png" ] =                                   psv2_psv2_pfd1;
    tempIconMap[ "2dicons/tvalve/tvalve_tvalve_tval1.png" ] =                            tvalve_tvalve_tval1;
    tempIconMap[ "2dicons/ise/ise_ise_icon1.png" ] =                                     ise_ise_icon1;
    tempIconMap[ "2dicons/sum/sum_sum_icon1.png" ] =                                     sum_sum_icon1;
    tempIconMap[ "2dicons/discretize/dicretize_discretize_icon1.png" ] =                 dicretize_discretize_icon1;
    tempIconMap[ "2dicons/iae/iae_iae_icon1.png" ] =                                     iae_iae_icon1;
    tempIconMap[ "2dicons/orifice/orifice_orifice_orifice1.png" ] =                      orifice_orifice_orifice1;
    tempIconMap[ "2dicons/deltap/deltap_deltap_dp.png" ] =                               deltap_deltap_dp;
    tempIconMap[ "2dicons/rcstr/rcstr_rcstr_horizontal.png" ] =                          rcstr_rcstr_horizontal;
    tempIconMap[ "2dicons/rcstr/rcstr_rcstr_vertical.png" ] =                            rcstr_rcstr_vertical;
    tempIconMap[ "2dicons/rcstr/rcstr_rcstr_sperical.png" ] =                            rcstr_rcstr_sperical;
    tempIconMap[ "2dicons/signalselector/selector_heat_selector_heat_heat.png" ] =       selector_heat_selector_heat_heat;
    tempIconMap[ "2dicons/signalselector/selector_work_selector_work_work.png" ] =       selector_work_selector_work_work;
    tempIconMap[ "2dicons/signalselector/signalselector_signalselector_icon1.png" ] =    signalselector_signalselector_icon1;
    tempIconMap[ "2dicons/pidincr/pidincr_pidincr_icon1.png" ] =                         pidincr_pidincr_icon1;
    tempIconMap[ "2dicons/teesplitter/teesplitter_teesplitter_dot.png" ] =               teesplitter_teesplitter_dot;
    tempIconMap[ "2dicons/teesplitter/teesplitter_teesplitter_tee.png" ] =               teesplitter_teesplitter_tee;
    tempIconMap[ "2dicons/mixer_heat/mixer_heat_mixer_heat_heat.png" ] =                 mixer_heat_mixer_heat_heat;
    tempIconMap[ "2dicons/signalgenerator/signalgenerator_signalgenerator_icon1.png" ] = signalgenerator_signalgenerator_icon1;
    tempIconMap[ "2dicons/signalgenerator/signalgenerator_signalgenerator_icon2.png" ] = signalgenerator_signalgenerator_icon2;
    tempIconMap[ "2dicons/ratio/ratio_ratio_icon1.png" ] =                               ratio_ratio_icon1;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_furnace.png" ] =                          rcstr3_rcstr3_furnace;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_v_drum2.png" ] =                          rcstr3_rcstr3_v_drum2;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_heater.png" ] =                           rcstr3_rcstr3_heater;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_h_drum.png" ] =                           rcstr3_rcstr3_h_drum;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_vertical.png" ] =                         rcstr3_rcstr3_vertical;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_horizontal.png" ] =                       rcstr3_rcstr3_horizontal;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_block.png" ] =                            rcstr3_rcstr3_block;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_icon1.png" ] =                            rcstr3_rcstr3_icon1;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_v_drum1.png" ] =                          rcstr3_rcstr3_v_drum1;
    tempIconMap[ "2dicons/rcstr3/rcstr3_rcstr3_sperical.png" ] =                         rcstr3_rcstr3_sperical;
    tempIconMap[ "2dicons/aplusfeed/aplusfeed_aplusfeed_arrow.png" ] =                   aplusfeed_aplusfeed_arrow;
    tempIconMap[ "2dicons/pipe3/pipe3_pipe3_b_pipe.png" ] =                              pipe3_pipe3_b_pipe;
    tempIconMap[ "2dicons/pipe3/pipe3_pipe3_u_pipe.png" ] =                              pipe3_pipe3_u_pipe;
    tempIconMap[ "2dicons/pipe3/pipe3_pipe3_v_pipe.png" ] =                              pipe3_pipe3_v_pipe;
    tempIconMap[ "2dicons/pipe3/pipe3_pipe3_d_pipe.png" ] =                              pipe3_pipe3_d_pipe;
    tempIconMap[ "2dicons/pipe3/pipe3_pipe3_expand.png" ] =                              pipe3_pipe3_expand;
    tempIconMap[ "2dicons/pipe3/pipe3_pipe3_block.png" ] =                               pipe3_pipe3_block;
    tempIconMap[ "2dicons/pipe3/pipe3_pipe3_h_pipe.png" ] =                              pipe3_pipe3_h_pipe;
    tempIconMap[ "2dicons/pipe3/pipe3_pipe3_contract.png" ] =                            pipe3_pipe3_contract;
    tempIconMap[ "2dicons/valve/valve_valve_icon1.png" ] =                               valve_valve_icon1;
    tempIconMap[ "2dicons/lead_lag/lead_lag_lead_lag_icon1.png" ] =                      lead_lag_lead_lag_icon1;
    tempIconMap[ "2dicons/dupl_work/dupl_work_dupl_work_work.png" ] =                    dupl_work_dupl_work_work;
    tempIconMap[ "2dicons/steamttop/steamttop_steamttop_icon1.png" ] =                   steamttop_steamttop_icon1;
    tempIconMap[ "2dicons/burstingdisk/burstingdisk_burstingdisk_bdisk1.png" ] =         burstingdisk_burstingdisk_bdisk1;
    tempIconMap[ "2dicons/teemixer/teemixer_teemixer_dot.png" ] =                        teemixer_teemixer_dot;
    tempIconMap[ "2dicons/teemixer/teemixer_teemixer_tee.png" ] =                        teemixer_teemixer_tee;
    return tempIconMap;
}