if(WIN32)
    add_definitions(-DQWT_DLL)
    add_definitions(-DQWT_MAKEDLL)
endif()
add_definitions(-DQWT_NO_SVG)

file( GLOB srcs "*.cpp" )
file( GLOB hdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.h" )

# workaround for conflicts between C++11 and the connect() macro
# in old Qt releases
# see "User-defined literals and whitespace" at:
# http://gcc.gnu.org/gcc-4.7/porting_to.html
if( CMAKE_COMPILER_IS_GNUCXX )
    set_source_files_properties( ${srcs}
                                 PROPERTIES
                                 COMPILE_FLAGS "-std=c++03" )
endif()

#set(VES_PRIVATE_HEADERS ${hdrs})
set(VES_PUBLIC_HEADERS ${hdrs} )

set(VES_TARGET_NAME qwt)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Lib)

include_directories(${QT_INCLUDE_DIRS})

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

SET( HDRS_TO_MOC ${hdrs})

list(REMOVE_ITEM HDRS_TO_MOC qwt.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_abstract_scale.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_abstract_scale_draw.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_arrow_button.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_clipper.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_color_map.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_column_symbol.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_compass_rose.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_compat.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_curve_fitter.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_dial_needle.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_double_range.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_event_pattern.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_global.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_interval.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_interval_symbol.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_legend_itemmanager.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_math.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_matrix_raster_data.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_painter.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_picker_machine.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_curve.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_dict.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_directpainter.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_grid.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_histogram.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_intervalcurve.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_item.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_layout.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_marker.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_rasteritem.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_rescaler.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_scaleitem.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_seriesitem.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_spectrocurve.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_spectrogram.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_plot_svgitem.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_point_3d.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_point_polar.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_raster_data.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_round_scale_draw.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_scale_div.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_scale_draw.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_scale_engine.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_scale_map.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_series_data.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_spline.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_symbol.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_system_clock.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_text.h)
list(REMOVE_ITEM HDRS_TO_MOC qwt_text_engine.h)

QT4_WRAP_CPP( MOCCED ${HDRS_TO_MOC} OPTIONS -p ./ )
list(APPEND srcs ${MOCCED})

add_library(${VES_TARGET_NAME} SHARED ${srcs} ${VES_PUBLIC_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${QT_LIBRARIES})

include(VESModuleInstall)

