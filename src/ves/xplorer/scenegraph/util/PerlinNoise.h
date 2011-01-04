/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
/************************************************************************
 *                                                                      *
 *                   Copyright (C) 2002  3Dlabs Inc. Ltd.               *
 *                                                                      *
 ***********************************************************************/
///This is modified code from 3DLabs
#ifndef PERLIN_NOISE_FUNCTIONS
#define  PERLIN_NOISE_FUNCTIONS
/*!\file PerlinNoise.h
Functions for creating Perlin Noise
*/
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
void SetNoiseFrequency( int frequency );

double noise1( double arg );
double noise2( double vec[2] );
double noise3( double vec[3] );
void normalize2( double vec[2] );
void normalize3( double vec[3] );

/*
   In what follows "alpha" is the weight when the sum is formed.
   Typically it is 2, As this approaches 1 the function is noisier.
   "beta" is the harmonic scaling/spacing, typically 2.
*/

double PerlinNoise1D( double x, double alpha, double beta, int n );
double PerlinNoise2D( double x, double y, double alpha, double beta, int n );
double PerlinNoise3D( double x, double y, double z, double alpha, double beta, int n );
}
}
}
}
#endif ///PERLIN_NOISE_FUNCTIONS
