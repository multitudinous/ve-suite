/* -*-c++-*-
*
*  OpenSceneGraph example, osgshaders.
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
*  THE SOFTWARE.
*/


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
namespace VE_SceneGraph
{
namespace Utilities
{
    void SetNoiseFrequency(int frequency);

    double noise1(double arg);
    double noise2(double vec[2]);
    double noise3(double vec[3]);
    void normalize2(double vec[2]);
    void normalize3(double vec[3]);

/*
   In what follows "alpha" is the weight when the sum is formed.
   Typically it is 2, As this approaches 1 the function is noisier.
   "beta" is the harmonic scaling/spacing, typically 2.
*/

    double PerlinNoise1D(double x,double alpha, double beta, int n);
    double PerlinNoise2D(double x,double y,double alpha, double beta, int n);
    double PerlinNoise3D(double x,double y,double z,double alpha, double beta, int n);
}
}
#endif ///PERLIN_NOISE_FUNCTIONS 
