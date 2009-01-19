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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef PARTICLE_H
#define PARTICLE_H
#include <vector>

class Data
{
public:
    Data( void );
    ~Data( void );
    Data( Data * );

    float time;
    float IDLIM;
    float particleCoalFraction;
    float particleCharFraction;
    float CPTREF;
    float particleTemperature;
    float gasTemperature;
    float particleSize;
    float particleCloudDispersion;
    float massOfChar;
    float moistureFraction;
};

class Location : public Data
{
public:
    Location( void );
    ~Location( void );
    Location( Location * );

    float x;
    float y;
    float z;
};

class Particle
{
public:
    Particle( void );
    ~Particle( void );
    Particle( Particle * );

    int pathIndex;
    int timeSteps;
    int particleNumber;
    int particleSize;
    typedef std::vector< Location * > Locations;
    Locations locations;
};
#endif
