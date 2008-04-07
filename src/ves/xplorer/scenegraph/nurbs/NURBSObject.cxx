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
#include <ves/xplorer/scenegraph/nurbs/NURBSObject.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/KnotVector.h>
#include <iostream>
#include <cmath>

using namespace ves::xplorer::scenegraph::nurbs;
////////////////////////
//Constructor         //
///////////////////////////////////////////////////////
NURBSObject::NURBSObject( Type type, unsigned int udegree,
                          unsigned int vdegree )
{
    _type = type;

    _nTotalControlPoints = 0;
    _needsRetessellation = true;
    //_currentSpan["U"].push_back( 0 );
    //_currentSpan["V"].push_back( 0 );

    _nControlPoints["U"] = 1;
    _nControlPoints["V"] = 1;

    _interpolationStepSize["U"] = .1;
    _interpolationStepSize["V"] = .1;

    _meshDimensions["U"] = 1;
    _meshDimensions["V"] = 1;


    _degree["U"] = udegree;
    _degree["V"] = vdegree;

    _order["U"] = _degree["U"] + 1;
    _order["V"] = _degree["V"] + 1;
    m_modifiedUBounds[0] = 0;
    m_modifiedUBounds[1] = 1;
    m_modifiedVBounds[0] = 0;
    m_modifiedVBounds[1] = 1;

}
/////////////////////////////////////////////
//Copy constructor                         //
/////////////////////////////////////////////
NURBSObject::NURBSObject( const NURBSObject& rhs )
{

    _type = rhs._type;
    _nTotalControlPoints = rhs._nTotalControlPoints;
    _nControlPoints = rhs._nControlPoints;
    _meshDimensions = rhs._meshDimensions;
    _needsRetessellation = rhs._needsRetessellation;
    _interpolationStepSize = rhs._interpolationStepSize;
    _currentSpan = rhs._currentSpan;
    _derivativeBasisFunctions = rhs._derivativeBasisFunctions;
    _knotDifferences = rhs._knotDifferences;
    _knotVectors = rhs._knotVectors;
    _uBasisFunctionsDerivatives = rhs._uBasisFunctionsDerivatives;
    _vBasisFunctionsDerivatives = rhs._vBasisFunctionsDerivatives;
    _controlPoints = rhs._controlPoints;
    _interpolatedPoints = rhs._interpolatedPoints;
    m_uvParameters = rhs.m_uvParameters;
    _parameterValues = rhs._parameterValues;
    m_uParameters = rhs.m_uParameters;
    m_vParameters = rhs.m_vParameters;

    //std::cout<<"Parameter values size copy objex: "<<_parameterValues["U"].size()<<std::endl;

    _degree = rhs._degree;

    _order = rhs._order;
    for( size_t i = 0; i <   m_changedVertexIndecies.size(); ++i )
    {
        m_changedVertexIndecies.push_back( rhs.m_changedVertexIndecies.at(i) );
    }

    m_modifiedUBounds[0] = rhs.m_modifiedUBounds[0];
    m_modifiedUBounds[1] = rhs.m_modifiedUBounds[1];
    m_modifiedVBounds[0] = rhs.m_modifiedVBounds[0];
    m_modifiedVBounds[1] = rhs.m_modifiedVBounds[1];
}
/////////////////////////
NURBSObject::~NURBSObject()
{
    _nControlPoints.clear();
    _interpolationStepSize.clear();
    _currentSpan.clear();
    _knotDifferences.clear();
    _knotVectors.clear();
    _uBasisFunctionsDerivatives.clear();
    _vBasisFunctionsDerivatives.clear();

    _controlPoints.clear();
    _interpolatedPoints.clear();

    _needsRetessellation = true;
    m_uvParameters.clear();
    m_changedVertexIndecies.clear();
}
////////////////////////////////////////////////////////
NURBSObject& NURBSObject::operator=( const NURBSObject& rhs )
{
    if( this != &rhs )
    {
        _type = rhs._type;

        _nControlPoints = rhs._nControlPoints;
        _nTotalControlPoints = rhs._nTotalControlPoints;
        _meshDimensions = rhs._meshDimensions;

        _needsRetessellation = rhs._needsRetessellation;
        _interpolationStepSize = rhs._interpolationStepSize;
        _parameterValues = rhs._parameterValues;
        m_uParameters = rhs.m_uParameters;
        m_vParameters = rhs.m_vParameters;

        //std::cout<<"Parameter values size equal objex: "<<_parameterValues["U"].size()<<std::endl;
        _currentSpan = rhs._currentSpan;
        _derivativeBasisFunctions = rhs._derivativeBasisFunctions;

        _knotDifferences = rhs._knotDifferences;

        _knotVectors = rhs._knotVectors;

        _uBasisFunctionsDerivatives = rhs._uBasisFunctionsDerivatives;
        _vBasisFunctionsDerivatives = rhs._vBasisFunctionsDerivatives;

        _controlPoints = rhs._controlPoints;
        _interpolatedPoints = rhs._interpolatedPoints;
        m_changedVertexIndecies.clear();
        for( size_t i = 0; i <   m_changedVertexIndecies.size(); ++i )
        {
            m_changedVertexIndecies.push_back( rhs.m_changedVertexIndecies.at(i) );
        }
        m_modifiedUBounds[0] = rhs.m_modifiedUBounds[0];
        m_modifiedUBounds[1] = rhs.m_modifiedUBounds[1];
        m_modifiedVBounds[0] = rhs.m_modifiedVBounds[0];
        m_modifiedVBounds[1] = rhs.m_modifiedVBounds[1];
    }
    return *this;
}
///////////////////////////////////////////////////////////////////////
void NURBSObject::SetDegree( unsigned int degree, std::string direction )
{
    _degree[direction] = degree;
    _order[direction] = _degree[direction] + 1;
    _needsRetessellation = true;
}
///////////////////////////////////////////////////////
void NURBSObject::SetKnotVector( ves::xplorer::scenegraph::nurbs::KnotVector knots,
                                 std::string direction )
{
    _knotVectors[direction] = knots;
    _needsRetessellation = true;
}
///////////////////////////////////////////////////////////////////////////
void NURBSObject::SetControlPoints( std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> ctrlPts,
                                    unsigned int columns,
                                    unsigned int rows )
{
    _controlPoints.clear();
    unsigned int row = 0;
    unsigned int column = 0;

    for( size_t i = 0; i < ctrlPts.size(); i++ )
    {
        ctrlPts.at( i ).SetRowColumnIndex( row, column );
        _controlPoints[0].push_back( ctrlPts.at( i ) );
        column++;

        if( column == columns )
        {
            column = 0;
            row++;
        }
    }
    _nControlPoints["U"] = columns;
    _nControlPoints["V"] = rows;
    _nTotalControlPoints = static_cast<unsigned int>( _controlPoints[0].size() );
    _needsRetessellation = true;
}
//////////////////////////////////////////////////////////
void NURBSObject::SetInterpolationGridSize( unsigned int stepSize,
                                            std::string direction )
{
    //if(stepSize < .5)
    //_interpolationStepSize[direction] = stepSize;
    _meshDimensions[direction] = stepSize;

    _needsRetessellation = true;
}
////////////////////////////////////////
NURBSObject::Type NURBSObject::GetType()
{
    return _type;
}
/////////////////////////////////////////////////////////////
void NURBSObject::SetMovingControlPoint( unsigned int index )
{
    ///This assumes the control point data has already been updated!!!
    double ubounds[2] = {0.0, 1.0};
    double vbounds[2] = {0.0, 1.0};

    /*for(std::map<double, unsigned int>::iterator iter = _parameterValues["V"].begin();
        iter != _parameterValues["V"].end();
        ++iter)
    {
        std::cout<<(*iter).first<<std::endl;
    }
    for(std::map<double, unsigned int>::iterator iter = _parameterValues["U"].begin();
        iter != _parameterValues["U"].end();
        ++iter)
    {
        std::cout<<(*iter).first<<std::endl;
    }*/
    ControlPoint modifiedControlPoint = _controlPoints[0][index];
    unsigned int vIndex = modifiedControlPoint.GetRowIndex();
    unsigned int uIndex = modifiedControlPoint.GetColumnIndex();

    //std::cout<<"Control Point index" <<std::endl;
    //std::cout<<uIndex<<" "<<vIndex<<std::endl;
    ubounds[0] = _knotVectors["U"].Knot( uIndex );
    ubounds[1] = _knotVectors["U"].Knot( uIndex + _degree["U"] + 1 );

    if( _type == ves::xplorer::scenegraph::nurbs::NURBSObject::Surface )
    {
        vbounds[0] = _knotVectors["V"].Knot( vIndex );
        vbounds[1] = _knotVectors["V"].Knot( vIndex + _degree["V"] + 1 );
        //std::cout<< uIndex + _degree["U"] + 1 <<" "<< vIndex + _degree["V"] + 1 <<std::endl;
    }
    //std::cout<<"Bound values:" <<std::endl;
    //std::cout<<ubounds[0]<<" "<<ubounds[1]<<std::endl;
    //std::cout<<vbounds[0]<<" "<<vbounds[1]<<std::endl;

    m_modifiedUBounds[0] = _findNearestParameterIndex( "U", ubounds[0] );
    m_modifiedUBounds[1] = _findNearestParameterIndex( "U", ubounds[1] );
    m_modifiedVBounds[0] = _findNearestParameterIndex( "V", vbounds[0] );
    m_modifiedVBounds[1] = _findNearestParameterIndex( "V", vbounds[1] );
    //std::cout<<"Modified bounds" <<std::endl;
    //std::cout<<m_modifiedUBounds[0]<<" "<<m_modifiedUBounds[1]<<std::endl;
    //std::cout<<m_modifiedVBounds[0]<<" "<<m_modifiedVBounds[1]<<std::endl;


    std::map<unsigned int, std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> > surfaceInfo;

    bool hasUderivative = ( _degree["U"] > 1 ) ? true : false;
    bool hasVderivative = ( _degree["V"] > 1 ) ? true : false;
    bool hasUVderivative = ( hasVderivative && hasUderivative ) ? true : false;

    m_changedVertexIndecies.clear();
    for( unsigned int v = m_modifiedVBounds[0]; v <= m_modifiedVBounds[1]; v++ )
    {
        for( unsigned int u = m_modifiedUBounds[0]; u <= m_modifiedUBounds[1]; u++ )
        {
            m_changedVertexIndecies.push_back( v*_meshDimensions["U"] + u );
     //       std::cout<<m_changedVertexIndecies.back()<<std::endl;
        }
    }
}
///////////////////////////////
void NURBSObject::UpdateMesh( )
{
    ///This assumes the control point data has already been updated!!!
    _interpolateWithinModifiedRange( );
}
/////////////////////////////////////////////////////////////////
void NURBSObject::UpdateControlPointPosition( unsigned int index,
                                              Point newPosition )
{
    _controlPoints[0][index].set( newPosition.x(), newPosition.y(), newPosition.z() );
}          
//////////////////////////////////////////////////////////////////////
unsigned int NURBSObject::NumInterpolatedPoints( std::string direction )
{
    return _meshDimensions[direction];
}
/////////////////////////////////////////////////////////////////
unsigned int NURBSObject::NumControlPoints( std::string direction )
{
    return _nControlPoints[direction];
}
//////////////////////////////////////////////////////////
unsigned int NURBSObject::GetDegree( std::string direction )
{
    return _degree[direction];
}
/////////////////////////////////////////////////////////
unsigned int NURBSObject::GetOrder( std::string direction )
{
    return _order[direction];
}
/////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::KnotVector& NURBSObject::KnotVector( std::string direction )
{
    return _knotVectors[direction];
}
/////////////////////////////////////////////////////////////////////////////////////
std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint>& NURBSObject::ControlPoints( unsigned int derivative )
{
    return _controlPoints[derivative];
}
////////////////////////////////////////////////////////////
std::vector<ves::xplorer::scenegraph::nurbs::Point>& NURBSObject::InterpolatedPoints()
{
    return _interpolatedPoints[0];
}
/////////////////////////////////////////////////////////////////////////////////////
std::vector< ves::xplorer::scenegraph::nurbs::Point > NURBSObject::GetUVParameters()
{
    return m_uvParameters;
}
//////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::ControlPoint* NURBSObject::GetControlPoint( size_t index )
{
    return &_controlPoints[0][index];
}
///////////////////////////////
void NURBSObject::Interpolate()
{
    if( !_needsRetessellation )
        return;
    if( !_controlPoints.size() )
    {
        std::cout << "No control points specified!!" << std::endl;
        std::cout << "NURBSObject::Interpolate()" << std::endl;
        return;
    }
}
////////////////////////////////////////////////////////////////////////////
void NURBSObject::_interpolateWithinBounds( double* uBounds, double* vBounds )
{

    //_interpolateWithinRange( uBounds[0], uBounds[1], vBounds[0], vBounds[1] );
    _interpolateWithinModifiedRange( );
}
////////////////////////////////////////////////////////////
void NURBSObject::_calculateBasisFunctions( double parameter,
                                            std::string direction )
{
    std::cout << "Not implemented!!" << std::endl;
    std::cout << "Use NURBSObject::_calculateBasisFunctionsAndDerivatives" << std::endl;
}
////////////////////////////////////////////////////////////////////////////
std::vector<unsigned int> NURBSObject::GetChangedTessellatedVertexIndecies()
{
    /*std::cout<<"NURBSObject::GetChangedTessellatedVertexIndecies"<<std::endl;
    std::vector<unsigned int>::iterator itr;
    for( itr = m_changedVertexIndecies.begin();
         itr != m_changedVertexIndecies.end();
         itr++ )
    {
        std::cout<<(*itr)<<std::endl;
    }*/

    return m_changedVertexIndecies;
}
/////////////////////////////////////
unsigned int NURBSObject::GetMinimumDegree()
{
    if( _type == NURBSObject::Curve )
    {
        return _degree["U"];
    }
    if( _degree["U"] < _degree["V"] )
        return _degree["U"];
    else
        return _degree["V"];
}
///////////////////////////////////////////////////////////////////////
unsigned int NURBSObject::_calculateBinomialCoefficients( unsigned int row,
                                                          unsigned int column )
{
    if (( row == 0 || column == 0 || row == column + 1 ) )
        return 1;
    return _calculateBinomialCoefficients( row -1, column - 1 )
           + _calculateBinomialCoefficients( row - 1, column );
}
/////////////////////////////////////////////////////////////////////////
void NURBSObject::_calculateBasisFunctionsAndDerivatives( double parameter,
                                                          unsigned int spanIndex,
                                                          std::string direction,
                                                          bool addToSpan )
{
    if( addToSpan )
    {
        _currentSpan[direction].push_back(  _knotVectors[direction].FindKnotSpan( parameter,
                              _degree[direction] ) );
    }
    //std::cout<<"Span["<<direction<<"]: "<<_currentSpan[direction].at(spanIndex)<<std::endl;


    _knotDifferences[direction][0][parameter].clear();
    _knotDifferences[direction][0][parameter].push_back( 1.0 );

    std::vector<double> left;
    std::vector<double> right;
    double saved = 0.0;
    double temp = 0.0;

    left.push_back( 0.0 );
    right.push_back( 0.0 );

    ///Compute the basis functions and derivatives -- Algo A2.3 Pigel
    for( size_t j = 1; j <= _degree[direction]; j++ )
    {

        left.push_back( parameter - _knotVectors[direction].Knot( _currentSpan[direction].at(spanIndex) + 1 - j ) );
        right.push_back( _knotVectors[direction].Knot( _currentSpan[direction].at(spanIndex) + j ) - parameter );

        saved = 0.0;
        temp = 0.0;

        for( size_t r = 0; r < j; r++ )
        {
            //Lower triangle for basis function table
            _knotDifferences[direction][j][parameter].push_back( right[r+1] + left[j-r] );
            temp = _knotDifferences[direction][r][parameter][j-1] / _knotDifferences[direction][j][parameter][r];


            //Upper triangle for basis function table
            _knotDifferences[direction][r][parameter].push_back( saved + ( right[r+1]*temp ) );
            saved = left[j-r] * temp;
        }
        _knotDifferences[direction][j][parameter].push_back( saved );
    }
    _derivativeBasisFunctions[direction][0][parameter].clear();

    //Initialize the "0th" derivative in our derivative map
    for( size_t j = 0; j <= _degree[direction]; j++ )
    {
        _derivativeBasisFunctions[direction][0][parameter].push_back( _knotDifferences[direction][j][parameter].at( _degree[direction] ) );
    }

    int row1 = 0;
    int row2 = 0;
    int rk = 0;
    int pk = 0;

    int jone = 0;
    int jtwo = 0;
    int tempRow = 0;

    unsigned int jthree = 0;
    double d = 0.0;
    double* a = new double [2*( _degree[direction] + 1 )];
    //Compute the derivatives
    for( int r = 0; r <= static_cast<int>( _degree[direction] ); r++ )
    {
        row1 = 0;
        row2 = 1;

        a[0] = 1.0;

        for( int k = 1; k <= static_cast<int>( _degree[direction] ); k++ )
        {
            d = 0.0;
            rk = r - k;
            pk = _degree[direction] - k;

            if( r >= k )
            {
                //a[s2][0] = a[s1][0]/ndu[pk+1][rk]
                a[row2*( _degree[direction] + 1 )] = a[row1*( _degree[direction] + 1 )] / _knotDifferences[direction][pk+1][parameter][rk];

                d = a[row2*( _degree[direction] + 1 )] * _knotDifferences[direction][rk][parameter][pk];
            }

            if( rk >= -1 )
            {
                jone = 1;
            }
            else
            {
                jone = ( -rk );
            }

            if( r - 1 <= pk )
            {
                jtwo = k - 1;
            }
            else
            {
                jtwo = _degree[direction] - r;
            }

            for( int j = jone; j <= jtwo; j++ )
            {
                a[row2*( _degree[direction] + 1 ) + j] = ( a[row1*( _degree[direction] + 1 ) + ( j )]
                                                           - a[row1*( _degree[direction] + 1 ) + ( j - 1 )] )
                                                         / _knotDifferences[direction][pk+1][parameter][rk+j];

                d += a[row2*( _degree[direction] + 1 ) + j] * _knotDifferences[direction][rk+j][parameter][pk];
            }

            if( r <= pk )
            {
                a[row2*( _degree[direction] + 1 ) + k] = -a[row1*( _degree[direction] + 1 ) + ( k - 1 )] / _knotDifferences[direction][pk+1][parameter][r];
                d += a[row2*( _degree[direction] + 1 ) + k] * _knotDifferences[direction][r][parameter][pk];
            }
            _derivativeBasisFunctions[direction][k][parameter].push_back( d );

            //check this if things go bad!!!
            tempRow = row1;
            row1 = row2;
            row2 = tempRow;
        }

    }
    int r = _degree[direction];
    for( int k = 1; k <= static_cast<int>( _degree[direction] ); k++ )
    {
        for( int j = 0; j <= int( _degree[direction] ); j++ )
        {
            _derivativeBasisFunctions[direction][k][parameter][j] *= r;
        }
        r *= (( _degree[direction] ) - k );
    }
    delete [] a;
}
//////////////////////////////////////////////////////////////////////////
unsigned int NURBSObject::_findNearestParameterIndex( std::string direction,
                                                      double parameter )
{
    std::map<double, unsigned int >::iterator lowerNearestValue;
    //endpoints
    /*if( parameter == 0.f )
    {
        std::cout<<"Parameter == 0: "<<parameter<<std::endl;
        std::cout<<"returning: "<<_parameterValues[direction].begin()->second<<std::endl;
        return _parameterValues[direction].begin()->second;
    }
    if( parameter == 1.f )
    {
        std::cout<<"Parameter == 1: "<<parameter<<std::endl;
        std::cout<<"returning: "<<_parameterValues[direction].rbegin()->second<<std::endl;
        return _parameterValues[direction].rbegin()->second;
    }*/

    lowerNearestValue = _parameterValues[direction].lower_bound( parameter );
    //std::cout<<"lowest nearest value: "<<lowerNearestValue->first<<std::endl;
    //std::cout<<"Parameter: "<<parameter<<std::endl;

    unsigned int counter = 0;
    if(lowerNearestValue == _parameterValues[direction].end())
    {
    //    std::cout<<"Invalid search" <<std::endl;
        if( parameter == 0.f )
        {
    //        std::cout<<"Parameter == 0: "<<parameter<<std::endl;
     //       std::cout<<"returning: "<<_parameterValues[direction].begin()->second<<std::endl;
            return _parameterValues[direction].begin()->second;
        }
        if( parameter == 1.f )
        {
      //      std::cout<<"Parameter == 1: "<<parameter<<std::endl;
       //     std::cout<<"returning: "<<_parameterValues[direction].rbegin()->second<<std::endl;
            return _parameterValues[direction].rbegin()->second;
        }
    }
    counter = 0;
    while (( float )parameter < ( float )lowerNearestValue->first /*&& counter < 20*/)
    {
        lowerNearestValue--;
        //std::cout<<"=== "<<lowerNearestValue->second<<std::endl;
        //++counter;
    }
    return lowerNearestValue->second;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> > NURBSObject::GetControlPoints( unsigned int derivative )
{
    size_t numUPoints = _nControlPoints["U"];
    size_t numVPoints = _nControlPoints["V"];
    std::vector< ves::xplorer::scenegraph::nurbs::ControlPoint > tempPoints = _controlPoints[ derivative ];
    std::vector< std::vector< ves::xplorer::scenegraph::nurbs::ControlPoint > > controlPoints;

    for( size_t i = 0; i < numVPoints; ++i )
    {
        std::vector< ves::xplorer::scenegraph::nurbs::ControlPoint > points;
        for( size_t j = 0; j < numUPoints; ++j )
        {
            points.push_back( tempPoints.at(( i * numUPoints ) + j ) );
        }
        controlPoints.push_back( points );
    }
    return controlPoints;
}

