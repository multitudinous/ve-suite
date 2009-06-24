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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Definitions.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/MatrixTransform>

// --- C/C++ Includes --- //
#include <vector>

namespace osg
{
class AutoTransform;
}

namespace osgUtil
{
class LineSegmentIntersector;
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
 * Abstract Class
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

    ///Override the addChild function to only accept Draggers
    virtual bool addChild( Dragger* child );

    ///
    ///\return
    virtual const char* className() const;

    ///
    void ComboForm();

    ///
    void DefaultForm();

    ///
    void Disable();

    ///
    void Enable();

    ///
    virtual Dragger* Focus( osg::NodePath::iterator& npItr );

    ///
    ///\return
    const std::vector< osg::Transform* >& GetAssociatedTransforms() const;

    ///
    ///Can't override the getChild function, so create our own
    ///\param i
    Dragger* GetChild( unsigned int i );

    ///Gets the transformation modes enabled on the manipulator
    const TransformationType::Enum GetEnabledModes() const;

    ///Gets the vector space in which the manipulator will operate
    const VectorSpace::Enum GetVectorSpace() const;

    ///Override the insertChild function to only accept Draggers
    virtual bool insertChild( unsigned int index, Dragger* child );

    ///
    const bool IsEnabled() const;

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* libraryName() const;

    ///
    virtual Dragger* Push(
        const osgUtil::LineSegmentIntersector& deviceInput,
        const osg::NodePath& np,
        osg::NodePath::iterator& npItr );


    ///
    virtual void PushBackAssociation(
        osg::Transform* transform, bool clearPreviousAssociations = false );

    ///
    virtual Dragger* Release( osg::NodePath::iterator& npItr );

    ///Override the replaceChild function to only accept Draggers
    virtual bool replaceChild( Dragger* origChild, Dragger* newChild );

    ///
    void SetAutoScaleToScreen( bool autoScaleToScreen );

    ///Override the setChild function to only accept Draggers
    virtual bool setChild( unsigned int i, Dragger* node );

    ///Sets the transformation modes enabled on the manipulator
    void SetEnabledModes( TransformationType::Enum value );

    ///Sets the vector space in which the manipulator will operate
    void SetVectorSpace( VectorSpace::Enum value ); 

    ///Deactivate the manipulator root
    void TurnOff();

    ///Activate the manipulator root
    void TurnOn();

protected:
    ///Destructor
    virtual ~Manipulator();

    ///Pure virtual
    virtual void SetupDefaultDraggers() = 0;

    ///
    TransformationType::Enum m_enabledModes;

    ///
    VectorSpace::Enum m_vectorSpace;

    ///
    bool m_enabled;

private:
    ///
    std::vector< osg::Transform* > m_associatedTransforms;

    ///
    osg::ref_ptr< osg::AutoTransform > m_autoTransform;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_H
