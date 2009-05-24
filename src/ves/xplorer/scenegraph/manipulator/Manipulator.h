/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef MANIPULATOR_H
#define MANIPULATOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Enums.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/MatrixTransform>

namespace osg
{
class AutoTransform;
}

// --- C/C++ Includes --- //
#include <map>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
class Dragger;

/*!\file Manipulator.h
 * Manipulator API
 */

/*!\class ves::xplorer::scenegraph::Manipulator
 *
 */
class VE_SCENEGRAPH_EXPORTS Manipulator : public osg::MatrixTransform
{
public:
    ///Constructor
    Manipulator();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Manipulator(
        const Manipulator& manipulator,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, Manipulator );

    ///Override the addChild function to only accept Draggers
    virtual bool addChild( Dragger* child );

    ///Can't override the getChild function, so create our own
    Dragger* GetChild( unsigned int i );

    ///
    virtual Dragger* Handle( Event::Enum event, osg::NodePath::iterator npItr );

    ///Override the insertChild function to only accept Draggers
    virtual bool insertChild( unsigned int index, Dragger* child );

    ///Override the replaceChild function to only accept Draggers
    virtual bool replaceChild( Dragger* origChild, Dragger* newChild );

    ///Override the setChild function to only accept Draggers
    virtual bool setChild( unsigned int i, Dragger* node );

    /*
    ///Gets the manipulator's active transformation mode
    const TransformationMode::Enum GetActiveMode() const;

    ///Gets the transformation modes enabled on the manipulator
    const TransformationMode::Enum GetEnabledModes() const;

    ///Gets the axes currently being operated on by the manipulator
    const AxesFlag::Enum GetSelectedAxes() const;

    ///Gets the vector space in which the manipulator will operate
    const VectorSpace::Enum GetVectorSpace() const;
    */

    ///
    void SetAutoScaleToScreen( bool autoScaleToScreen );

    /*
    ///Sets the transformation modes enabled on the manipulator
    void SetEnabledModes( TransformationMode::Enum value );

    ///Sets the vector space in which the manipulator will operate
    void SetVectorSpace( VectorSpace::Enum value ); 
    */

    ///Deactivate the manipulator root
    void TurnOff();

    ///Activate the manipulator root
    void TurnOn();

protected:
    ///Destructor
    virtual ~Manipulator();

    ///
    ///Can't use pure virtual with META_Node define
    virtual void SetupDefaultDraggers();// = 0;

    /*
    ///
    TransformationMode::Enum m_activeMode;
    
    ///
    TransformationMode::Enum m_enabledModes;
    
    ///
    AxesFlag::Enum m_selectedAxes;
    
    ///
    VectorSpace::Enum m_vectorSpace;
    */

    ///
    bool m_manipulating;

private:
    ///
    osg::ref_ptr< osg::AutoTransform > m_autoTransform;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //MANIPULATOR_H
