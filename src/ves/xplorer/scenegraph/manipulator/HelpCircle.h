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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_HELP_CIRCLE_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_HELP_CIRCLE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class ClipNode;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
/*!\file HelpCircle.h
 * HelpCircle API
 */

/*!\class ves::xplorer::scenegraph::HelpCircle
 *
 */
class VE_SCENEGRAPH_EXPORTS HelpCircle : public Dragger
{
public:
    ///
    HelpCircle();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    HelpCircle(
        const HelpCircle& rotateTwist,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    ///\return
    virtual const char* className() const;

    ///
    ///\param copyop
    ///\return
    virtual osg::Object* clone( const osg::CopyOp& copyop ) const;

    ///
    ///\return
    virtual osg::Object* cloneType() const;

    ///
    const osg::ClipNode* const GetClipNode() const;

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    virtual Dragger* Release( osg::NodePath::iterator& npItr );

protected:
    ///
    virtual ~HelpCircle();

    ///
    virtual void SetupDefaultGeometry();

private:
    ///
    osg::ref_ptr< osg::ClipNode > m_clipNode;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_HELP_CIRCLE_H
