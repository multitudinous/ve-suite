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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_COMPOUND_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_COMPOUND_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/CompoundDragger.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
class TranslateAxis;
class TranslatePan;
class Manipulator;

/*!\file TranslateCompound.h
 * TranslateCompound API
 */

/*!\class ves::xplorer::scenegraph::TranslateCompound
 *
 */
class VE_SCENEGRAPH_EXPORTS TranslateCompound : public CompoundDragger
{
public:
    ///Constructor
    TranslateCompound(
        Manipulator* const parentManipulator );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    TranslateCompound(
        const TranslateCompound& translateCompound,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    ///\param nv
    virtual void accept( osg::NodeVisitor& nv );

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
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* libraryName() const;

    ///
    virtual void ComboForm();

    ///
    virtual void DefaultForm();

protected:
    ///Destructor
    virtual ~TranslateCompound();

    ///
    virtual void SetupDefaultGeometry();

private:
    ///
    const double m_explodeDistance;

    ///
    osg::ref_ptr< TranslateAxis > m_xTranslateAxis;

    ///
    osg::ref_ptr< TranslateAxis > m_yTranslateAxis;

    ///
    osg::ref_ptr< TranslateAxis > m_zTranslateAxis;

    ///
    osg::ref_ptr< TranslatePan > m_translatePan;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_COMPOUND_H
