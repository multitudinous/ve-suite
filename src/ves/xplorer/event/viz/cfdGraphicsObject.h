/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#ifndef CFD_GRAPHICS_OBJECT_H
#define CFD_GRAPHICS_OBJECT_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/ModelPtr.h>

#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Geode.h>
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- C/C++ Libraries --- //
#include <vector>

#include <latticefx/core/vtk/DataSetPtr.h>
#include <latticefx/core/PlayControl.h>

namespace osg
{
class PositionAttitudeTransform;
}

namespace ves
{
namespace xplorer
{
class cfdObjects;

namespace scenegraph
{
class Group;
class Geode;
///class DCS;
}

/*!\file cfdGraphicsObject.h
 * cfdGraphicsObject API
 *\class ves::xplorer::cfdGraphicsObject
 *
 */
class VE_XPLORER_EXPORTS cfdGraphicsObject
{
public:
    ///constructor
    cfdGraphicsObject();

    ///destructor
    ~cfdGraphicsObject();

    ///copy constructor
    ///\param &input
    cfdGraphicsObject( const cfdGraphicsObject& input );

    ///equal operator
    ///\param &input
    cfdGraphicsObject& operator=( const cfdGraphicsObject& input );

    enum VizType
    {
        TRANSIENT, TEXTURE, CLASSIC, LFX, OTHER
    };///<types of viz objects possible to add to scene

    ///Set parent node to add "graphics node" to
    ///\param input
    void SetParentNode( osg::PositionAttitudeTransform* const input );

    ///node the parent node will be added to
    ///\param input
    void SetWorldNode( ves::xplorer::scenegraph::DCS* const input );

    ///set model pointer to be able to grab
    ///transient info and the switch node
    ///\param input
    void SetActiveModel( Model* const input );

    ///Set the dataset used for this viz options
    ///\param dataset The dataset for this viz object
    void SetDataSet( lfx::core::vtk::DataSetPtr const dataset );

    ///Get dataset for this viz option
    lfx::core::vtk::DataSetPtr GetDataSet();

    ///add "child node" to scene graph
    void AddGraphicsObjectToSceneGraph();

    ///Set type of viz: trans, classic, texture
    ///\param VizType
    void SetTypeOfViz( VizType );

    ///Set geodes for classic and trans viz objects
    void SetGeodes( ves::xplorer::cfdObjects* const input );

    ///Return parent node for a this object
    osg::PositionAttitudeTransform* GetParentNode();

    ///Clear geodes vector and geode from memory and the graph
    void RemoveGeodeFromDCS();

    ///Used to enable the animated streamlines
    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > >& GetGeodes();

    ///Set the uui for the current feature
    void SetUUID( std::string const& uuid );

    ///Set the uui for the current feature
    std::string const& GetUUID() const;

    ///Determine whether this is a transient dataset
    bool IsTransient() const;

    ///Update the any per frame data
    void PreFrameUpdate();

protected:
    ///SceneGraph Geode
    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > geodes;
    osg::ref_ptr< osg::PositionAttitudeTransform > parentNode;///<SceneGraph parent node.
    ves::xplorer::scenegraph::DCS* worldNode;///<SceneGraph world node.
    VizType type;///<Type of viz: trans, classic, texture.

    // used for animated particles and other ss
    // animated features
    osg::ref_ptr< osg::Sequence > m_animation;
    ves::xplorer::Model* model;///<Xplorer cfd model.
    ///The dataset used for this viz option
    lfx::core::vtk::DataSetPtr m_dataset;
    ///The uuid for the current feature
    std::string m_uuid;
    ///lfx viz group node
    osg::ref_ptr< osg::Node > m_lfxGroup;
    ///Flag to determine transient datasets
    bool m_transient;
    ///Control lfx animations
    lfx::core::PlayControlPtr m_playControl;
};
}
}

#endif // end CFD_GRAPHICS_OBJECT_H
