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
 * Date modified: $Date: 2009-05-06 14:32:42 -0600 (Wed, 06 May 2009) $
 * Version:       $Rev: 12657 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: Dragger.h 12657 2009-05-06 20:32:42Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef DRAGGER_H
#define DRAGGER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Enums.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/Drawable>

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
/*!\file Dragger.h
 * Dragger API
 */

/*!\class ves::xplorer::scenegraph::Dragger
 *
 */
class VE_SCENEGRAPH_EXPORTS Dragger : public osg::MatrixTransform
{
public:
    ///
    Dragger();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Dragger(
        const Dragger& dragger,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, Dragger );

    ///
    ///Can't use pure virtual with META_Node define
    virtual bool Handle( Event::Enum event, osg::NodePath::iterator npItr );

    ///
    virtual void SetColor(
        ColorTag::Enum colorTag, osg::Vec4 newColor, bool use = false );

    ///Activate the dragger
    void TurnOn();

    ///Deactivate the dragger
    void TurnOff();

    ///
    virtual void UseColor( ColorTag::Enum colorTag );

protected:
    ///
    virtual ~Dragger();

    ///
    ///Can't use pure virtual with META_Node define
    virtual void SetupDefaultGeometry();// = 0;

    ///
    void SetDrawableToAlwaysCull( osg::Drawable& drawable );

private:
    ///
    typedef std::map< ColorTag::Enum, osg::Vec4 > ColorMap;

    ///
    class ForceCullCallback : public osg::Drawable::CullCallback
    {
    public:
        ///
        ForceCullCallback();

        ///
        ForceCullCallback(
            const ForceCullCallback& forceCullCallback,
            const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

        ///
        META_Object(
            ves::xplorer::scenegraph::manipulator::Dragger, ForceCullCallback );

        ///
        virtual bool cull(
            osg::NodeVisitor* nv,
            osg::Drawable* drawable,
            osg::RenderInfo* renderInfo ) const;

    protected:

    private:

    };

    ///
    void CreateDefaultShader();

    ///
    osg::Vec4& GetColor( ColorTag::Enum colorTag );

    ///
    ColorMap m_colorMap;

    ///
    osg::ref_ptr< osg::Uniform > m_color;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //DRAGGER_H
