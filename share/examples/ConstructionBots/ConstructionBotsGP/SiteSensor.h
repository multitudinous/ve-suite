/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#ifndef SITE_SENSOR_H
#define SITE_SENSOR_H

// --- My Includes --- //
#include "Sensor.h"

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/LineSegment>

#include <osgUtil/IntersectVisitor>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

// --- C/C++ Libraries --- //
#include <vector>

//Simulates an optical type sensor
namespace bots
{
class SiteSensor : public Sensor
{
public:
    SiteSensor( bots::AgentEntity* agentEntity );

    virtual ~SiteSensor();

    virtual void CollectInformation();

    void Rotate();

    void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );
    void RemoveLine();

    bool SiteInView();
    bool CloseToSite();

    btVector3 GetNormalizedSiteVector();

    void SetRange( double range );

private:
    void Initialize();

    bool m_siteInView;
    bool m_closeToSite;

    double m_angle;
    double m_angleInc;
    double m_range;

    btVector3 m_normalizedSiteVector;

    osg::ref_ptr< osg::Geometry > m_line;
    osg::ref_ptr< osg::Geode > m_beamGeode;
    osg::ref_ptr< osg::LineSegment > m_beamLineSegment;

};
} //end bots

#endif //SITE_SENSOR_H
