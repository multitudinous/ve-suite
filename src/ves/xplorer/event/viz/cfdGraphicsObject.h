/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_GRAPHICS_OBJECT_H
#define CFD_GRAPHICS_OBJECT_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/ModelPtr.h>

#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Geode.h>
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/ref_ptr>
#endif

// --- C/C++ Libraries --- //
#include <vector>

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

    enum VizType{TRANSIENT,TEXTURE,CLASSIC,OTHER};///<types of viz objects possible to add to scene

    ///Set parent node to add "graphics node" to
    ///\param input
    void SetParentNode( ves::xplorer::scenegraph::DCS* input );

    ///node the parent node will be added to
    ///\param input
    void SetWorldNode( ves::xplorer::scenegraph::DCS* input );

    ///set model pointer to be able to grab
    ///transient info and the switch node
    ///\param input
    void SetActiveModel( Model* input );

    ///add "child node" to scene graph
    void AddGraphicsObjectToSceneGraph();

    ///Set type of viz: trans, classic, texture
    ///\param VizType
    void SetTypeOfViz( VizType );

    ///Set geodes for classic and trans viz objects
    void SetGeodes( ves::xplorer::cfdObjects* input );

    ///Return parent node for a this object
    ves::xplorer::scenegraph::DCS* GetParentNode();

    ///Clear geodes vector and geode from memory and the graph
    void RemoveGeodeFromDCS();

    // Return the animation so that we can change the speed of the animation
    //ves::xplorer::scenegraph::cfdTempAnimation* GetAnimation();

    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > GetGeodes();

protected:
    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > geodes;///<SceneGraph Geode.
    ves::xplorer::scenegraph::DCS* parentNode;///<SceneGraph parent node.
    ves::xplorer::scenegraph::DCS* worldNode;///<SceneGraph world node.
    VizType type;///<Type of viz: trans, classic, texture.

    // used for animated particles and other ss 
    // animated features
    //ves::xplorer::scenegraph::cfdTempAnimation* animation;
    ves::xplorer::Model* model;///<Xplorer cfd model.
};
}
}

#endif // end CFD_GRAPHICS_OBJECT_H
