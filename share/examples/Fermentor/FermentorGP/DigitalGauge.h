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

#ifndef DIGITAL_GAUGE_H
#define DIGITAL_GAUGE_H

// --- My Includes --- //
class Shaders;

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/MatrixTransform>

namespace osgText
{
    class Text;
}

// --- C/C++ Libraries --- //
#include <string>

namespace display
{
class VE_USER_PLUGIN_EXPORTS DigitalGauge : public osg::MatrixTransform
{
public:
    DigitalGauge();
    DigitalGauge( std::string name );

    META_Node( display, DigitalGauge );

protected:
    ///Destructor
    virtual ~DigitalGauge();

    ///Copy constructor
    DigitalGauge( const DigitalGauge& digitalGauge, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    //Assignment operator
    DigitalGauge &operator=( const DigitalGauge &digitalGauge );

public:
    void Initialize();

    void UpdateText( double value );

    void SetPrecision( int precision );

    osgText::Text* GetNameText();

    osgText::Text* GetDigitalText();

private:
    int m_precision;
    
    std::string m_name;

    Shaders* m_shader;

    osg::ref_ptr< osgText::Text > m_nameText;
    osg::ref_ptr< osgText::Text > m_digitalText;
};
} // end display

#endif //DIGITAL_GAUGE_H
