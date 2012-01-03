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

namespace ves
{
namespace xplorer
{
class DataSet;
class cfdObjects;

namespace scenegraph
{
class Group;
class Geode;
///class DCS;
}

/*!\file cfdGraphicsObject.h
 * cfdGraphicsObject API
 */

/*!\class ves::xplorer::cfdGraphicsObject
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
        TRANSIENT, TEXTURE, CLASSIC, OTHER
    }
    ;///<types of viz objects possible to add to scene

    ///Set parent node to add "graphics node" to
    ///\param input
    void SetParentNode( ves::xplorer::scenegraph::DCS* const input );

    ///node the parent node will be added to
    ///\param input
    void SetWorldNode( ves::xplorer::scenegraph::DCS* const input );

    ///set model pointer to be able to grab
    ///transient info and the switch node
    ///\param input
    void SetActiveModel( Model* const input );

    ///Set the dataset used for this viz options
    ///\param dataset The dataset for this viz object
    void SetDataSet( ves::xplorer::DataSet* const dataset );

    ///Get dataset for this viz option
    ves::xplorer::DataSet* GetDataSet();

    ///add "child node" to scene graph
    void AddGraphicsObjectToSceneGraph();

    ///Set type of viz: trans, classic, texture
    ///\param VizType
    void SetTypeOfViz( VizType );

    ///Set geodes for classic and trans viz objects
    void SetGeodes( ves::xplorer::cfdObjects* const input );

    ///Return parent node for a this object
    ves::xplorer::scenegraph::DCS* GetParentNode();

    ///Clear geodes vector and geode from memory and the graph
    void RemoveGeodeFromDCS();

    ///Used to enable the animated streamlines
    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > >& GetGeodes();

    ///Set the uui for the current feature
    void SetUUID( std::string const& uuid );
    
    ///Set the uui for the current feature
    std::string const& GetUUID() const;
        
protected:
    ///SceneGraph Geode
    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > geodes;
    ves::xplorer::scenegraph::DCS* parentNode;///<SceneGraph parent node.
    ves::xplorer::scenegraph::DCS* worldNode;///<SceneGraph world node.
    VizType type;///<Type of viz: trans, classic, texture.

    // used for animated particles and other ss
    // animated features
    osg::ref_ptr< osg::Sequence > m_animation;
    ves::xplorer::Model* model;///<Xplorer cfd model.
    ///The dataset used for this viz option
    ves::xplorer::DataSet* m_dataset;
    ///The uuid for the current feature
    std::string m_uuid;
};
}
}

#endif // end CFD_GRAPHICS_OBJECT_H
