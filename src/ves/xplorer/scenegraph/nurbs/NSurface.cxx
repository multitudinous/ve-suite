/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#include <ves/xplorer/scenegraph/nurbs/NSurface.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/KnotVector.h>
#include <iostream>
#include <cmath>

using namespace ves::xplorer::scenegraph::nurbs;
////////////////////////
//Constructor         //
////////////////////////
NURBSSurface::NURBSSurface( unsigned int udegree,
                            unsigned int vdegree )
        : NURBSObject( NURBSObject::Surface, udegree, vdegree )
{
    _needsRetessellation = true;
}
/////////////////////////////////////////////
//Copy constructor                         //
/////////////////////////////////////////////
NURBSSurface::NURBSSurface( const NURBSSurface& rhs )
: NURBSObject( rhs )
{
    _surfDerivatives = rhs._surfDerivatives;
}
/////////////////////////
NURBSSurface::~NURBSSurface()
{
    _surfDerivatives.clear();
}
//////////////////////////////////////////////////////////////
NURBSSurface& NURBSSurface::operator=( const NURBSSurface& rhs )
{
    if( this != &rhs )
    {
        NURBSObject::operator=( rhs );
        _surfDerivatives = rhs._surfDerivatives;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////
void NURBSSurface::_interpolateWithinBounds( double* uBounds, double* vBounds )
{
    _interpolateWithinModifiedRange( );
}
///////////////////////////////////////////////////////////////////
void NURBSSurface::_interpolateWithinModifiedRange( /*double umin, double umax,
                                            double vmin, double vmax*/ )
{

    //This function assumes all the proper checks have been made
    //before entering!!!!!
    double uparam = 0;//umin;
    double vparam = 0;//vmin;

    unsigned int uIndexMin = m_modifiedUBounds[0];
    unsigned int uIndexMax = m_modifiedUBounds[1];
    unsigned int vIndexMin = m_modifiedVBounds[0];
    unsigned int vIndexMax = m_modifiedVBounds[1];

    std::map<unsigned int, std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> > surfaceInfo;

    bool hasUderivative = ( _degree["U"] > 1 ) ? true : false;
    bool hasVderivative = ( _degree["V"] > 1 ) ? true : false;
    bool hasUVderivative = ( hasVderivative && hasUderivative ) ? true : false;
    //std::cout<<"index bounds: "<<uIndexMin<<" "<<uIndexMax<<std::endl;
    //std::cout<<"index bounds: "<<vIndexMin<<" "<<vIndexMax<<std::endl;
    unsigned int vIndexBound = (vIndexMax-vIndexMin+1);
    unsigned int uIndexBound = (uIndexMax-uIndexMin+1);
    unsigned int numberOfVertsToUpdate = ( vIndexBound )*( uIndexBound );
    unsigned int u = 0;
    unsigned int v = 0;
    for( unsigned int nv = 0; nv < numberOfVertsToUpdate; ++nv )
    {
        u = uIndexMin + ( nv + uIndexMin )% uIndexBound;
        v = vIndexMin +( (nv + uIndexMin) / uIndexBound) % vIndexBound;
        uparam = m_uParameters.at(u);
        vparam = m_vParameters.at(v);
        surfaceInfo = _calculatePointOnSurface( uparam, vparam, _currentSpan["U"].at(u),
                                                                    _currentSpan["V"].at(v) );

        _interpolatedPoints[0][v*_meshDimensions["U"] + u] = surfaceInfo[0].at( 0 );
        //S(u,v)
        _surfDerivatives[0][0][v*_meshDimensions["U"] + u] = surfaceInfo[0].at( 0 );
        if( hasUVderivative )
        {
            //dS/dv
            _surfDerivatives[0][1][v*_meshDimensions["U"] + u] = surfaceInfo[0].at( 1 );

            //dS/dU
            _surfDerivatives[1][0][v*_meshDimensions["U"] + u] = surfaceInfo[1].at( 0 );
        }
     //   std::cout<<"u,v: ("<<u<<","<<v<<")"<<std::endl;
    }
    /*for( unsigned int v = vIndexMin; v <= vIndexMax; v++ )
    {
        //std::cout<<_parameterValues["V"][v];
        vparam = m_vParameters.at(v);
        //std::cout<<m_vParameters.at(v);
        for( unsigned int u = uIndexMin; u <= uIndexMax; u++ )
        {
            //uparam = _parameterValues["U"][u];
            uparam = m_uParameters.at(u);
            surfaceInfo = _calculatePointOnSurface( uparam, vparam, _currentSpan["U"].at(u),
                                                                    _currentSpan["V"].at(v) );

            _interpolatedPoints[0][v*_meshDimensions["U"] + u] = surfaceInfo[0].at( 0 );
            //S(u,v)
            _surfDerivatives[0][0][v*_meshDimensions["U"] + u] = surfaceInfo[0].at( 0 );
            if( hasUVderivative )
            {
                //dS/dv
                _surfDerivatives[0][1][v*_meshDimensions["U"] + u] = surfaceInfo[0].at( 1 );

                //dS/dU
                _surfDerivatives[1][0][v*_meshDimensions["U"] + u] = surfaceInfo[1].at( 0 );
*/
                /*try---these aren't used for anything now
                {
                   //ds/dudv
                   _surfDerivatives[1][1][v*_meshDimensions["U"]+ u] =surfaceInfo[1].at(1);
                }
                catch(...)
                {
                    ///just means we have a lower degree surface (<3)
                }*/
            /*}
        }
    }*/
}
////////////////////////////////
void NURBSSurface::Interpolate()
{
    if( !_needsRetessellation )
        return;
    if( !_controlPoints[0].size() )
    {
        std::cout << "No control points specified!!" << std::endl;
        std::cout << "NURBSSurface::Interpolate()" << std::endl;
        return;
    }

    if( !_knotVectors["U"].NumberOfKnots() )
    {
        std::cout << "No U knots specified!!" << std::endl;
        std::cout << "NURBSSurface::Interpolate()" << std::endl;
        return;
    }
    if( !_knotVectors["V"].NumberOfKnots() )
    {
        std::cout << "No V knots specified!!" << std::endl;
        std::cout << "NURBSSurface::Interpolate()" << std::endl;
        return;
    }
    //Check our U surface conditions eq 3.12 NURBS Book
    //r + 1 = (n + 1) + (p + 1)
    SetDegree( static_cast<unsigned int>( _knotVectors["U"].NumberOfKnots() - _nControlPoints["U"] ) - 1, "U" );

    //ensure our v surface conditions
    //s + 1 = (m + 1) + (q + 1)
    SetDegree( static_cast<unsigned int>( _knotVectors["V"].NumberOfKnots() - _nControlPoints["V"] ) - 1, "V" );

    bool hasUderivative = ( _degree["U"] > 1 ) ? true : false;
    bool hasVderivative = ( _degree["V"] > 1 ) ? true : false;
    bool hasUVderivative = ( hasVderivative && hasUderivative ) ? true : false;
    _interpolatedPoints.clear();

    _parameterValues.clear();
    m_uParameters.clear();
    m_vParameters.clear();
    m_uvParameters.clear();

    double uparam = 0.0;
    double vparam = 0.0;

    //reduce the number of approximation points 
    //if the KnotVector contains no interior points
    if( !_knotVectors["U"].HasInteriorKnots() )
    {
        _meshDimensions["U"] = _nControlPoints["U"]; 
    }
    if( !_knotVectors["V"].HasInteriorKnots() )
    {
        _meshDimensions["V"] = _nControlPoints["V"];
    }

    _interpolationStepSize["U"] = 1.0 / ( _meshDimensions["U"] - 1 );
    _interpolationStepSize["V"] = 1.0 / ( _meshDimensions["V"] - 1 );
    bool addToSpan = false;

    std::map<unsigned int, std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> > surfaceInfo;
    //std::cout<<"Is this being called multiple time?!!!!"<<std::endl;
    for( unsigned int v = 0;v < _meshDimensions["V"]; v++ )
    {
        addToSpan = false;
        _parameterValues["V"][vparam] = v;
        m_vParameters.push_back( vparam );
        _calculateBasisFunctionsAndDerivatives( vparam,v,"V" );
        for( unsigned int u = 0;u < _meshDimensions["U"]; u++ )
        {
            if( !v )
            {
                _parameterValues["U"][uparam] = u;
                m_uParameters.push_back( uparam );
                addToSpan = true;
            }
            _calculateBasisFunctionsAndDerivatives( uparam,u,"U",addToSpan );

            surfaceInfo = _calculatePointOnSurface( uparam, vparam,
                                                    _currentSpan["U"].at(u),
                                                    _currentSpan["V"].at(v) );
            _interpolatedPoints[0].push_back( surfaceInfo[0].at( 0 ) );
            m_uvParameters.push_back( ves::xplorer::scenegraph::nurbs::Point( uparam, vparam, 0 ) );

            //S(u,v)
            _surfDerivatives[0][0].push_back( surfaceInfo[0].at( 0 ) );
            if( hasUVderivative )
            {
                //dS/du
                _surfDerivatives[0][1].push_back( surfaceInfo[0].at( 1 ) );

                //dS/dV
                _surfDerivatives[1][0].push_back( surfaceInfo[1].at( 0 ) );

                /*try---these aren't used for anything now
                {
                   //ds/dudv
                   _surfDerivatives[1][1].push_back(surfaceInfo[1].at(1));
                }
                catch(...)
                {
                    //just means we have a lower degree (less than 3)surface
                }*/
            }
            uparam += _interpolationStepSize["U"];
        }

        uparam = 0.0;
        vparam += _interpolationStepSize["V"];
    }
    //std::cout<<"ParameterValues size Interpolate: "<<_parameterValues["U"].size()<<" "<<_parameterValues["V"].size()<<std::endl;
}
////////////////////////////////////////////////////////////////
std::map<unsigned int, std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> >
 NURBSSurface::_calculatePointOnSurface( double u,
        double v,
        unsigned int uspan,
        unsigned int vspan )
{
    std::map<unsigned int, std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> > resutlingWeightedPoint;//(0,0,0);
    std::map<unsigned int, std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> > aDerivatives;
    std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> tempUContribution;
    unsigned int uindex = 0;
    unsigned int vindex = 0;
    double invWeight = 0.0;///resutlingWeightedPoint.Weight();

    double ctrlPtWeight = 1.0;
    double ucontrib [4] = {0., 0., 0., 0.0};
    unsigned int udegree = _degree["U"];
    unsigned int vdegree = _degree["V"];
    for( unsigned int n = 0; n < udegree; n++ )
    {
        tempUContribution.clear();
        for( unsigned int l = 0; l <= vdegree; l++ )
        {
            //vindex = _currentSpan["V"].at(v) - vdegree + l;
            vindex = vspan - vdegree + l;
            //std::cout<<vindex<<" "<<vspan<<std::endl;
            ucontrib[0] = 0;
            ucontrib[1] = 0;
            ucontrib[2] = 0;
            ucontrib[3] = 0.0;
            for( unsigned int k = 0; k <= udegree; k++ )
            {
                //uindex = _currentSpan["U"].at(u) - udegree + k;
                uindex = uspan - udegree + k;
                //std::cout<<uindex<<" "<<uspan<<std::endl;

                ucontrib[0] += ( _controlPoints[0][vindex*_nControlPoints["U"] + uindex].WeightedX()
                                 * _derivativeBasisFunctions["U"][n][u].at( k ) );
                ucontrib[1] += ( _controlPoints[0][vindex*_nControlPoints["U"] + uindex].WeightedY()
                                 * _derivativeBasisFunctions["U"][n][u].at( k ) );
                ucontrib[2] += ( _controlPoints[0][vindex*_nControlPoints["U"] + uindex].WeightedZ()
                                 * _derivativeBasisFunctions["U"][n][u].at( k ) );
                ucontrib[3] += _controlPoints[0][vindex*_nControlPoints["U"] + uindex].Weight()
                               * _derivativeBasisFunctions["U"][n][u].at( k );
            }
            //invWeight = 1.0/ucontrib[3];
            tempUContribution.push_back( ves::xplorer::scenegraph::nurbs::ControlPoint( ucontrib[0]/*invWeight*/,
                                         ucontrib[1]/*invWeight*/,
                                         ucontrib[2]/*invWeight*/,
                                         ucontrib[3] ) );
        }

        double sw [4] = {0., 0., 0., 0.0};
        for( unsigned int j = 0; j < vdegree; j++ )
        {
            sw[0] = 0;
            sw[1] = 0;
            sw[2] = 0;
            sw[3] = 0.0;
            for( unsigned int l = 0; l <= vdegree; l++ )
            {
                sw[0] += ( tempUContribution[l].X()
                           * _derivativeBasisFunctions["V"][j][v].at( l ) );
                sw[1] += ( tempUContribution[l].Y()
                           * _derivativeBasisFunctions["V"][j][v].at( l ) );
                sw[2] += ( tempUContribution[l].Z()
                           * _derivativeBasisFunctions["V"][j][v].at( l ) );
                sw[3] += ( tempUContribution[l].Weight()
                           * _derivativeBasisFunctions["V"][j][v].at( l ) );
            }
            aDerivatives[n].push_back( ControlPoint( sw[0], sw[1], sw[2], sw[3] ) );
        }
    }
    ///only calculate the 1st derivative for now
    unsigned int d = GetMinimumDegree() - 1;
    double vContrib[3] = {0., 0., 0.};
    double v2[3] = {0., 0., 0.};
    unsigned int bcoeff = 1;
    for( unsigned int k = 0; k <= d;k++ )
    {
        for( unsigned int l = 0; l <= d - k;l++ )
        {
            vContrib[0] = aDerivatives[k][l].X();
            vContrib[1] = aDerivatives[k][l].Y();
            vContrib[2] = aDerivatives[k][l].Z();
            for( unsigned int j = 1; j <= l; j++ )
            {
                bcoeff = _calculateBinomialCoefficients( l, j );
                vContrib[0] -= ( bcoeff * aDerivatives[0][j].Weight()
                                 * resutlingWeightedPoint[k][l-j].X() );

                vContrib[1] -= ( bcoeff * aDerivatives[0][j].Weight()
                                 * resutlingWeightedPoint[k][l-j].Y() );

                vContrib[2] -= ( bcoeff * aDerivatives[0][j].Weight()
                                 * resutlingWeightedPoint[k][l-j].Z() );
            }
            for( unsigned int i = 1; i <= k; i++ )
            {
                bcoeff = _calculateBinomialCoefficients( k, i );
                vContrib[0] -= ( bcoeff * aDerivatives[i][0].Weight()
                                 * resutlingWeightedPoint[k-i][l].X() );

                vContrib[1] -= ( bcoeff * aDerivatives[i][0].Weight()
                                 * resutlingWeightedPoint[k-i][l].Y() );

                vContrib[2] -= ( bcoeff * aDerivatives[i][0].Weight()
                                 * resutlingWeightedPoint[k-i][l].Z() );

                v2[0] = 0.0;
                v2[1] = 0.0;
                v2[2] = 0.0;
                for( unsigned int g = 1; g <= l; g++ )
                {
                    bcoeff = _calculateBinomialCoefficients( l, g );
                    v2[0] += ( bcoeff * aDerivatives[i][g].Weight()
                               * resutlingWeightedPoint[k-i][l-g].X() );

                    v2[1] += ( bcoeff * aDerivatives[i][g].Weight()
                               * resutlingWeightedPoint[k-i][l-g].Y() );

                    v2[2] += ( bcoeff * aDerivatives[i][g].Weight()
                               * resutlingWeightedPoint[k-i][l-g].Z() );

                }
                bcoeff = _calculateBinomialCoefficients( k, i );
                vContrib[0] -= bcoeff * v2[0];
                vContrib[1] -= bcoeff * v2[1];
                vContrib[2] -= bcoeff * v2[2];
            }
            resutlingWeightedPoint[k].push_back( ves::xplorer::scenegraph::nurbs::ControlPoint( vContrib[0] / aDerivatives[0][0].Weight(),
                                                 vContrib[1] / aDerivatives[0][0].Weight(),
                                                 vContrib[2] / aDerivatives[0][0].Weight() ) );
        }
    }

    //std::cout<<resutlingWeightedPoint[0][0]<<std::endl;
    return resutlingWeightedPoint;
}
//////////////////////////////////////////////////////////////////////////
std::map<unsigned int, std::map<unsigned int, std::vector<ves::xplorer::scenegraph::nurbs::Point> > >
NURBSSurface::GetSurfaceDerivatives()
{
    return _surfDerivatives;
}
////////////////////////////////////////////////////////////////////////////////
void NURBSSurface::Write( std::ostream& stream )
{
    ves::xplorer::scenegraph::nurbs::KnotVector uKnotVector = this->KnotVector( "U" );
    ves::xplorer::scenegraph::nurbs::KnotVector vKnotVector = this->KnotVector( "V" );
    std::vector< unsigned int > uMultiplicity = uKnotVector.GetMultiplicityVector();
    std::vector< double > uKnots = uKnotVector.GetDistinctKnotVector();
    std::vector< unsigned int > vMultiplicity = vKnotVector.GetMultiplicityVector();
    std::vector< double > vKnots = vKnotVector.GetDistinctKnotVector();

    // get the uknots
    stream << " Knots U " << std::endl;
    for( size_t i = 0; i < uKnots.size(); ++i )
    {
        for( size_t j = 0; j < uMultiplicity.at( i ); ++j )
        {
            stream << uKnots.at( i ) << " ";
        }
    }
    stream << std::endl;
    // get the vknots
    stream << " Knots V " << std::endl;
    for( size_t i = 0; i < vKnots.size(); ++i )
    {
        for( size_t j = 0; j < vMultiplicity.at( i ); ++j )
        {
            stream << vKnots.at( i ) << " ";
        }
    }
    stream << std::endl;

    //Get poles and weights
    std::vector< std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> > points = this->GetControlPoints();
    double X;
    double Y;
    double Z;
    double W;
    stream << " Poles & Weights " << std::endl;
    for( size_t j = 0; j < points.size(); ++j )
    {
        for( size_t i = 0; i < points.at( j ).size(); ++i )
        {
            X = points.at( j ).at( i ).X();
            Y = points.at( j ).at( i ).Y();
            Z = points.at( j ).at( i ).Z();
            W = points.at( j ).at( i ).Weight();
            stream << "( " << X << " " << Y
            << " " << Z << " " << W << " ) ";
        }
        stream << std::endl;
    }
}
