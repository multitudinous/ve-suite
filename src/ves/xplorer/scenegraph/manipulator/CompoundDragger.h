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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_COMPOUND_DRAGGER_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_COMPOUND_DRAGGER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //


namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
/*!\file CompoundDragger.h
 * CompoundDragger API
 */

/*!\class ves::xplorer::scenegraph::CompoundDragger
 * Abstract Class
 */
class VE_SCENEGRAPH_EXPORTS CompoundDragger : public Dragger
{
public:
    ///
    CompoundDragger(
        const AxesFlag::Enum& axesFlag,
        const TransformationType::Enum& transformationType );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    CompoundDragger(
        const CompoundDragger& compoundDragger,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    void ComboForm();

    ///
    void DefaultForm();

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* className() const;

    ///Override the addChild function to only accept Draggers
    virtual bool addChild( Dragger* child );

    ///
    virtual Dragger* Focus( osg::NodePath::iterator& npItr );

    ///Can't override the getChild function, so create our own
    Dragger* GetChild( unsigned int i );

    ///
    //virtual void Hide();

    ///
    virtual Dragger* Push(
        const osgUtil::LineSegmentIntersector& deviceInput,
        const osg::NodePath& np,
        osg::NodePath::iterator& npItr );

    ///
    virtual Dragger* Release( osg::NodePath::iterator& npItr );

    ///Override the insertChild function to only accept Draggers
    virtual bool insertChild( unsigned int index, Dragger* child );

    ///Override the replaceChild function to only accept Draggers
    virtual bool replaceChild( Dragger* origChild, Dragger* newChild );

    ///Override the setChild function to only accept Draggers
    virtual bool setChild( unsigned int i, Dragger* node );

    ///
    virtual void SetColor(
        Color::Enum colorTag, osg::Vec4& newColor, bool use = false );

    ///Sets the transformation modes enabled on the manipulator
    void SetEnabledModes( TransformationType::Enum value );

    ///
    virtual void SetVectorSpace( const VectorSpace::Enum& vectorSpace );

    ///
    //virtual void Show();

    ///
    virtual void UseColor( Color::Enum colorTag );

protected:
    ///
    virtual ~CompoundDragger();

    ///Pure virtual again
    ///
    virtual void SetupDefaultGeometry() = 0;

    ///
    TransformationType::Enum m_enabledModes;

private:

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_COMPOUND_DRAGGER_H
