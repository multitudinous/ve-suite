/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_ENUM_H
#define CFD_ENUM_H
//! Enumerator
/*!
  Type of cursors.
*/
enum cursorTYPE
{
    XPLANE,
    YPLANE,
    ZPLANE,
    SPHERE,
    ARROW,
    CUBE,
    XLINE,
    YLINE,
    ZLINE,
    NONE
};
//! Enumerator
/*!
  Enumerate the pfGeode objects for each cfd visualization.
*/
enum cfdGeodeEnum
{
    // Everything below has to be mirrored exactly in
    // VE_Suite/VE_Xplorer/UMI/hello/config/mapping.config
    CONTOUR, X_CONTOUR, Y_CONTOUR, Z_CONTOUR,
    X_CONTOURS, Y_CONTOURS, Z_CONTOURS,
    MOMENTUM, X_MOMENTUM, Y_MOMENTUM, Z_MOMENTUM,
    X_MOMENTUMS, Y_MOMENTUMS, Z_MOMENTUMS,
    VECTOR, X_VECTOR, Y_VECTOR, Z_VECTOR,
    X_VECTORS, Y_VECTORS, Z_VECTORS,
    STREAMLINES, ISOSURFACE,
    IMAGE_EX,   //yang-REI: changed due to conflict with /usr/include/Performer/image.h
    POLYDATA,
    SWITCH_CURSOR,
    ANIMATED_STREAMLINES,
    ANIMATED_IMAGES,
    PARTICLES,
    TRANS_GEOM,
    X_TRANSIENT_CONTOUR,
    Y_TRANSIENT_CONTOUR,
    Z_TRANSIENT_CONTOUR,
    X_TRANSIENT_VECTOR,
    Y_TRANSIENT_VECTOR,
    Z_TRANSIENT_VECTOR,
    X_TRANSIENT_CONTOUR_AND_VECTOR,
    Y_TRANSIENT_CONTOUR_AND_VECTOR,
    Z_TRANSIENT_CONTOUR_AND_VECTOR,
    PARTICLE_TRANSIENT,
    // Everything below has to be mirrored exactly in
    // VE_Suite/VE_Xplorer/UMI/hello/config/mapping2.config
    // The first of the following non-geode related commands specifies an
    // offset: this offset must be the same as that used to set up the
    // instance of id_mapper class called "my_mapper2" in client.java
    CHANGE_SCALAR = 100,
    CHANGE_SCALAR_RANGE, //101
    UPDATE_GEOMETRY,//102
    SEND_DRAW,//103
    UPDATE_SEND_PARAM,//104
    RECORD_SCENE,//105
    CLEAR_ALL,//106
    SET_TRANSIENT_OPTIONS,
    TRANSIENT_RESET,
    TRANSIENT_BACKWARD,
    LOAD_PFB_FILE,//110
    CLEAR_PFB_FILE,
    TRANSIENT_FORWARD,
    TRANSIENT_STOP,
    COMPUTE_STREAMLINES,
    USE_LAST_STREAMLINE_SEEDPOINTS,
    CHANGE_STREAMLINE_CURSOR,
    NO_CURSOR,
    POINT_CURSOR,
    X_LINE_CURSOR,
    Y_LINE_CURSOR, //120
    Z_LINE_CURSOR,
    X_PLANE_CURSOR,
    Y_PLANE_CURSOR,
    Z_PLANE_CURSOR,
    SCALAR_BAR_TOGGLE,
    CHANGE_STEADYSTATE_DATASET,
    CHANGE_VECTOR,
    CHANGE_VECTOR_THRESHOLD,
    CHANGE_VECTOR_MASK_RATIO,
    CHANGE_VECTOR_SCALE,//130
    SCALE_BY_VECTOR_MAGNITUDE,
    BACKWARD_INTEGRATION,
    FORWARD_INTEGRATION,
    TWO_DIRECTION_INTEGRATION,
    CHANGE_PROPAGATION_TIME,
    CHANGE_INT_STEP_LENGTH,
    CHANGE_STEP_LENGTH,
    STREAMLINE_DIAMETER,
    STREAMLINE_ARROW,
    CHANGE_CONTOUR_FILL, //140
    UPDATE_SOUNDS,//141
    CHANGE_PARTICLE_VIEW_OPTION,
    //CHANGE_SPHERE_SIZE, //Not used, functionality taken care of with CHANGE_PARTICLE_VIEW_OPTION
    //QuatCam-Flythrough operations
    LOAD_NEW_VIEWPT,
    MOVE_TO_SELECTED_LOCATION,
    REMOVE_SELECTED_VIEWPT,
    ADD_NEW_POINT_TO_FLYTHROUGH,
    INSERT_NEW_POINT_IN_FLYTHROUGH,
    REMOVE_POINT_FROM_FLYTHROUGH,
    DELETE_ENTIRE_FLYTHROUGH,
    ADD_NEW_FLYTHROUGH, //150
    RUN_ACTIVE_FLYTHROUGH,
    STOP_ACTIVE_FLYTHROUGH,
    CHANGE_MOVEMENT_SPEED,
    MIRROR_VIS_DATA,
    EXIT,
    //biv--added these for the navigation page
    GUI_NAV,
    NAV_UP,
    NAV_DOWN,
    NAV_LEFT,
    NAV_RIGHT, //160
    NAV_FWD,
    NAV_BKWD,
    PITCH_DOWN,
    PITCH_UP,
    ROLL_CW,
    ROLL_CCW,
    YAW_CW,
    YAW_CCW,
    CHANGE_ACTIVE_MODEL,
    ACT_CUSTOM_VIZ, //170
    TRANSIENT_VIS_ACTIVE,
    CHANGE_TRANSLATION_STEP_SIZE,
    CHANGE_ROTATION_STEP_SIZE,
    ROTATE_ABOUT_HEAD,
    RESET_NAVIGATION_POSITION,
    CHANGE_CONTOUR_SETTINGS,
    CHANGE_LOD_SCALE,
    TRANSIENT_ACTIVE,
    CHANGE_NAVIGATION_STEP_SIZE,
    NAV_CW,
    NAV_CCW,

    //biv -- these are for texture based visualization
    VIS_OPTION,
    TEXTURE_BASED_VISUALIZATION,
    CLASSIC_VISUALIZATION,
    SHOW_TEXTURE_BBOX,
    TEXTURE_BASED_SHADERS,
    ADVECTION_SHADER,
    VOLUME_SHADER,
    DYE_TRANSLATION,
    NOISE_SCALE,
    WEIGHT,
    TRANSIENT_PLAY,
    TRANSIENT_DURATION,
    TRANSIENT_SET_FRAME,
    GEOMETRY_PICKING,

    // Added for voice commands
    RESET_PICKED_GEOMETRY,
    SELECT_GEOMETRY,
    UNSELECT_GEOMETRY,
    GOTO_X,
    GOTO_Y,
    GOTO_Z

};
#endif
