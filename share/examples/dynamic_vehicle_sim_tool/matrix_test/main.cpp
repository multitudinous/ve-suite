/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VR Juggler is (C) Copyright 1998-2010 by Iowa State University
 *
 * Original Authors:
 *   Allen Bierbaum, Christopher Just,
 *   Patrick Hartling, Kevin Meinert,
 *   Carolina Cruz-Neira, Albert Baker
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
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <gmtl/VecOps.h>
#include <gmtl/gmtl.h>
#include <gmtl/Point.h>
#include <gmtl/Misc/MatrixConvert.h>

gmtl::Matrix44d GetLookAtMatrix( gmtl::Point4d& frontPoint, gmtl::Point4d& leftRear, gmtl::Point4d& rightRear )
{
    //Create the centroid for the triangle
    //centroid = ((pt1[0]+pt2[0]+pt3[0])/3.0,
    //            (pt1[1]+pt2[1]+pt3[1])/3.0,
    //            (pt1[2]+pt2[2]+pt3[2])/3)
    double factor = 1.0;

    gmtl::Point3d centroid;
    centroid.set( 
                 factor * (frontPoint[ 0 ] + leftRear[ 0 ] + rightRear[ 0 ])/3.0, 
                 factor * (frontPoint[ 1 ] + leftRear[ 1 ] + rightRear[ 1 ])/3.0,
                 factor * (frontPoint[ 2 ] + leftRear[ 2 ] + rightRear[ 2 ])/3.0 );
    
    //Create vector from the origin of the triangle to the front bird
    gmtl::Vec3d forwardVec;
    forwardVec.set( frontPoint[ 0 ] - centroid[ 0 ], 
                   (frontPoint[ 1 ] - centroid[ 1 ]), 
                   frontPoint[ 2 ] - centroid[ 2 ] );
    gmtl::normalize( forwardVec );
    
    //Cross the front vector with the vector from one of the rear bird corners
    gmtl::Vec3d rearVec;
    rearVec.set( rightRear[ 0 ] - centroid[ 0 ], 
                (rightRear[ 1 ] - centroid[ 1 ]), 
                rightRear[ 2 ] - centroid[ 2 ] );
    gmtl::normalize( rearVec );
    
    //Create the up vector
    gmtl::Vec3d upVec;
    upVec.set( (rearVec[1]*forwardVec[2]) - (rearVec[2]*forwardVec[1]),
              (rearVec[2]*forwardVec[0]) - (rearVec[0]*forwardVec[2]),
              (rearVec[0]*forwardVec[1]) - (rearVec[1]*forwardVec[0]) );
    gmtl::normalize( upVec );
    
    gmtl::Vec3d rightVec;
    rightVec.set( (forwardVec[1]*upVec[2]) - (forwardVec[2]*upVec[1]),
                 (forwardVec[2]*upVec[0]) - (forwardVec[0]*upVec[2]),
                 (forwardVec[0]*upVec[1]) - (forwardVec[1]*upVec[0]) );
    gmtl::normalize( rightVec );
    
    //GMTL is columan major order so this is why the data is laid out in columns
    //http://www.fastgraph.com/makegames/3drotation/
    ///With this matrix setup we are making Y forward. 
    gmtl::Matrix44d transMat;
    transMat.set( rightVec[ 0 ], forwardVec[ 0 ], upVec[ 0 ],  centroid[ 0 ],
                 rightVec[ 1 ], forwardVec[ 1 ], upVec[ 1 ],  centroid[ 1 ],
                 rightVec[ 2 ], forwardVec[ 2 ], upVec[ 2 ],  centroid[ 2 ],
                 0.,         0.,              0.,             1. );
    
    //This link also helps detail this matrix construction
    //http://www.puregamer.co.uk/blog/vector-rotation-matrix-generation.html
    
    return transMat;
}


int main( int argc, char* argv[])
{
    if( argc < 1 )
    {
        std::cout << " What ! " << argv[ 0 ] << std::endl;
        return 1;
    }
    //0.643805 4.71268 -3.03138
    gmtl::Point4d headPoint;
    headPoint.set( 0.643805, 4.71268, -3.03138, 1.0 );
    gmtl::Point4d frontBird( headPoint[ 0 ], -headPoint[ 2 ], headPoint[ 1 ], headPoint[ 3 ] );
    
    //-0.803867 5.09858 2.33167
    gmtl::Point4d wandPoint;
    wandPoint.set( -0.803867, 5.09858, 2.33167, 1.0 );
    gmtl::Point4d leftRearBird( wandPoint[ 0 ], -wandPoint[ 2 ], wandPoint[ 1 ], wandPoint[ 3 ] );
    
    //1.96846 5.09418 2.34925    
    gmtl::Point4d pointerPoint;
    pointerPoint.set( 1.96846, 5.09418, 2.34925, 1.0 );
    gmtl::Point4d rightRearBird( pointerPoint[ 0 ], -pointerPoint[ 2 ], pointerPoint[ 1 ], pointerPoint[ 3 ] );
    std::cout << "The delta in the rear bird data (ft) " << leftRearBird - rightRearBird << std::endl;
    
    ///Get the lookat matrix based on the bird points
    gmtl::Matrix44d transMat = GetLookAtMatrix( frontBird, leftRearBird, rightRearBird );
    std::cout << "Bird coord " << std::endl << transMat << std::endl << std::flush;
    
    ///The forward vector for the cad
    gmtl::Vec3d m_forwardVector;
    m_forwardVector.set( 1, 0, 0 );
    m_forwardVector *= -1.0f;

    ///The up vector for the cad
    gmtl::Vec3d m_upVector;
    m_upVector.set( 0, 0, 1 );                

    ///Now lets setup the CAD specific matrix to let the dvst know what the
    ///orientation of the CAD is.
    gmtl::Vec3d rightCADVec;
    rightCADVec.set( 
                    (m_forwardVector[1]*m_upVector[2]) - (m_forwardVector[2]*m_upVector[1]),
                    (m_forwardVector[2]*m_upVector[0]) - (m_forwardVector[0]*m_upVector[2]),
                    (m_forwardVector[0]*m_upVector[1]) - (m_forwardVector[1]*m_upVector[0]) );
    gmtl::normalize( rightCADVec );
    
    gmtl::Matrix44d cadOrientationMat;
    cadOrientationMat.set( 
                          rightCADVec[ 0 ], m_forwardVector[ 0 ], m_upVector[ 0 ],  0.,
                          rightCADVec[ 1 ], m_forwardVector[ 1 ], m_upVector[ 1 ],  0.,
                          rightCADVec[ 2 ], m_forwardVector[ 2 ], m_upVector[ 2 ],  0.,
                          0.,                   0.,              0.,  1. );
    
    //Get the SIP offsets from the birds to the centroid
    //X is to the rear, y is up, z is to the left
    //fbirdd = (sip[0]*u.mm-1048.1*u.mm,sip[1]*u.mm+686.8*u.mm,sip[2]*u.mm+13.3*u.mm)
	//lrbirdd = (sip[0]*u.mm+597.8*u.mm,sip[1]*u.mm+792.5*u.mm,sip[2]*u.mm+421.4*u.mm)
	//rrbirdd = (sip[0]*u.mm+600.9*u.mm,sip[1]*u.mm+792.4*u.mm,sip[2]*u.mm-421.4*u.mm)
    double mm2ft = 0.0032808;
    
    double frontBirdX = -1048.1 * mm2ft;
     double frontBirdY =  686.8 * mm2ft;
     double frontBirdZ =   13.3 * mm2ft;
     
     double leftRearBirdX = 597.8 * mm2ft;
     double leftRearBirdY = 792.5 * mm2ft;
     double leftRearBirdZ = 421.4 * mm2ft;
     
     double rightRearBirdX =  600.9 * mm2ft;
     double rightRearBirdY =  792.4 * mm2ft;
     double rightRearBirdZ = -421.4 * mm2ft;
    /*
    double frontBirdX = m_birdData.at( 0 ) * mm2ft;
    double frontBirdY = m_birdData.at( 1 ) * mm2ft;
    double frontBirdZ = m_birdData.at( 2 ) * mm2ft;
    
    double leftRearBirdX = m_birdData.at( 3 ) * mm2ft;
    double leftRearBirdY = m_birdData.at( 4 ) * mm2ft;
    double leftRearBirdZ = m_birdData.at( 5 ) * mm2ft;
    
    double rightRearBirdX = m_birdData.at( 6 ) * mm2ft;
    double rightRearBirdY = m_birdData.at( 7 ) * mm2ft;
    double rightRearBirdZ = m_birdData.at( 8 ) * mm2ft;*/
    
    //These coords are transformed into ves coord space
    //x = -z
    //y = -x
    //z = y
    gmtl::Point4d sipOffSetFrontBird;
    sipOffSetFrontBird.set( -frontBirdZ, -frontBirdX, frontBirdY, 1.0 );
    
    gmtl::Point4d sipOffSetLeftRearBird;
    sipOffSetLeftRearBird.set( -leftRearBirdZ, -leftRearBirdX, leftRearBirdY, 1.0 );
    
    gmtl::Point4d sipOffSetRightRearBird;
    sipOffSetRightRearBird.set( -rightRearBirdZ, -rightRearBirdX, rightRearBirdY, 1.0 );
    
    ///Get the lookat matrix based on the bird calibration points
    gmtl::Matrix44d measuredSIPCentroidMat = 
    GetLookAtMatrix( sipOffSetFrontBird, sipOffSetLeftRearBird, sipOffSetRightRearBird );
    
    ///This code is used if the user would like to transform a measured point in space
    ///rather than create a delta transform as we do belows
    //gmtl::Point3d measuredSIPCentroid;
    //measuredSIPCentroid.set( 
    //    -1.0 * (sipOffSetFrontBird[ 0 ] + sipOffSetLeftRearBird[ 0 ] + sipOffSetRightRearBird[ 0 ])/3.0, 
    //    -1.0 * (sipOffSetFrontBird[ 1 ] + sipOffSetLeftRearBird[ 1 ] + sipOffSetRightRearBird[ 1 ])/3.0,
    //    -1.0 * (sipOffSetFrontBird[ 2 ] + sipOffSetLeftRearBird[ 2 ] + sipOffSetRightRearBird[ 2 ])/3.0 );
    //std::cout << "Bird data " << measuredSIPCentroid << std::endl << std::flush;
    //gmtl::Matrix44d measuredSIPCentroidTransMat = 
    //    gmtl::makeTrans< gmtl::Matrix44d >( measuredSIPCentroid );
    
    std::cout << "Bird data " << std::endl << measuredSIPCentroidMat << std::endl << std::flush;
    
    
    gmtl::Point3d m_sip;
    m_sip.set( -3.03, 0.0f, 4.17 );

    ///SIP location from the user on the UI
    gmtl::Matrix44d sipLoc = gmtl::makeTrans< gmtl::Matrix44d >( m_sip );
    std::cout << "Measured SIP " << m_sip << std::endl << std::endl << std::flush;

    std::cout << "CAD SIP orientation matrix " << std::endl 
        << cadOrientationMat << std::endl << std::flush;
        
    cadOrientationMat = sipLoc * cadOrientationMat;
    std::cout << "Base SIP coordinate matrix " << std::endl 
        << cadOrientationMat << std::endl << std::flush;

    measuredSIPCentroidMat = cadOrientationMat * measuredSIPCentroidMat;
    std::cout << "Measured SIP with orientation " << std::endl 
        << measuredSIPCentroidMat << std::endl << std::flush;
    
    //measuredSIPCentroidMat = sipLoc * measuredSIPCentroidMat * cadOrientationMat; //maybe
    //| -0.019475 0.997889 -0.0619511 1.1201 |
    //| -0.997243 -0.0149501 0.0726819 -4.07211 |
    //| 0.0716023 0.0631958 0.995429 -1.42703 |
    //measuredSIPCentroidMat = measuredSIPCentroidMat * sipLoc * cadOrientationMat; //probably not
    //| -0.019475 0.997889 -0.0619511 0.877683 |
    //| -0.997243 -0.0149501 0.0726819 -4.06648 |
    //| 0.0716023 0.0631958 0.995429 -1.43658 |

    //measuredSIPCentroidMat = sipLoc * cadOrientationMat * measuredSIPCentroidMat; //yes
    //| -0.0195433 0.999809 0.000431068 0.558475 |
    //| -0.999783 -0.019546 0.00725811 -3.46311 |
    //| 0.00726515 -0.000289127 0.999974 -1.66486 |    
    
    //measuredSIPCentroidMat = cadOrientationMat * sipLoc * measuredSIPCentroidMat; //no - wrong coordinate frame
    //measuredSIPCentroidMat = cadOrientationMat * measuredSIPCentroidMat * sipLoc; //no - wrong coordinate frame
    //measuredSIPCentroidMat = measuredSIPCentroidMat * cadOrientationMat * sipLoc; //no - wrong coordinate frame
    //measuredSIPCentroidMat = sipLoc * measuredSIPCentroidMat;
    //std::cout << "Measured SIP with offset " 
    //    << std::endl << measuredSIPCentroidMat << std::endl << std::flush;
        
    //std::cout << "CAD orientation matrix " << std::endl 
    //    << cadOrientationMat << std::endl << std::flush;

    //measuredSIPCentroidMat = measuredSIPCentroidMat * cadOrientationMat ;
    //std::cout << "Measured SIP with orientation " << std::endl 
    //    << measuredSIPCentroidMat << std::endl << std::flush;
    
    /*gmtl::Point4d point;
    point.set( 0.0f, 0.0f, 0.0f, 1.0f );
    measuredSIPCentroidMat = measuredSIPCentroidMat * point ;
    std::cout << "Measured SIP with orientation " << std::endl 
    << measuredSIPCentroidMat << std::endl << std::flush;*/

    //#ifndef DVST_TEST
    //Now we convert the sip matrix back through the transform mat to move it 
    //to the VR Juggler coord
    
    ///Invert this so that we can create a transform between the two lookat
    ///matrices we createds
    gmtl::invert( measuredSIPCentroidMat );
    
    gmtl::Matrix44d registerMat = transMat * measuredSIPCentroidMat;
    
    std::cout << "Reg matrix " << std::endl 
        << registerMat << std::endl << std::flush;
    
    ///Set the registration matrix
    //gmtl::Matrix44d m_initialNavMatrix = registerMat;// * cadOrientationMat * sipLoc;
    //std::cout << "Init nav matrix with CAD correction" << std::endl 
    //    << m_initialNavMatrix << std::endl << std::flush;
    
    
    std::cout << "Close answer" << std::endl 
        <<  " Init nav matrix with CAD correction \n"
        <<  "| 0.060949 0.998118 -0.00674674 1.55312 |\n"
        <<  "| -0.995422 0.0612801 0.0733472 -4.0873 |\n"
        <<  "| 0.0736226 0.00224541 0.997284 -1.45508 |\n"
        <<  "| 0 0 0 1 | "<< std::endl;

    return 0;
}

/*
13 Oct 2011
Bird coord 
| 0.999927 0.0118934 0.00202524 0.602188 |
| -0.0120077 0.997377 0.0713751 -0.551434 |
| -0.00117103 -0.0713942 0.997447 4.96836 |
| 0 0 0 1 |

Bird data 
| 0.999967 -0.00805627 0.000353818 -0.0145449 |
| 0.00801711 0.997918 0.0640008 -0.164696 |
| -0.000868689 -0.0639959 0.99795 2.48433 |
| 0 0 0 1 |

Measured SIP (-3.03, 0, 4.17)
Bird data 
| 0.999967 -0.00805627 0.000353818 -3.04454 |
| 0.00801711 0.997918 0.0640008 -0.164696 |
| -0.000868689 -0.0639959 0.99795 6.65433 |
| 0 0 0 1 |

Bird data 
| 0.00801711 0.997918 0.0640008 -0.164696 |
| -0.999967 0.00805627 -0.000353818 3.04454 |
| -0.000868689 -0.0639959 0.99795 6.65433 |
| 0 0 0 1 |

Reg matrix 
| 0.0200248 0.997823 0.0628318 -0.811982 |
| -0.997187 0.0244728 -0.0708395 3.45706 |
| -0.072223 -0.0612365 0.995507 1.93678 |
| 0 0 0 1 |

CAD orientation matrix 
| 0 1 0 0 |
| -1 0 0 0 |
| 0 0 1 0 |
| 0 0 0 1 |

Init nav matrix with CAD correction
| 0.0200248 0.997823 0.0628318 0.811982 |
| -0.997187 0.0244728 -0.0708395 -3.45706 |
| -0.072223 -0.0612365 0.995507 -1.93678 |
| 0 0 0 1 |
*/

/*
15 Aug 2011
 Reg matrix 
 | 0.999998 -0.000914867 0.00160781 0.307847 |
 | 0.000789897 0.997102 0.0760785 -1.12003 |
 | -0.00167275 -0.0760771 0.997101 2.49588 |
 | 0 0 0 1 |
 
 Bird data (0.0145449, 0.164696, -2.48433)
 
 Reg matrix 
 | 0.998118 -0.060949 -0.00674674 1.34031 |
 | 0.0612801 0.995422 0.0733472 -0.765309 |
 | 0.00224541 -0.0736226 0.997284 2.48051 |
 | 0 0 0 1 |
 
 Init nav matrix 
 | 0.998118 -0.060949 -0.00674674 1.34031 |
 | 0.0612801 0.995422 0.0733472 -0.765309 |
 | 0.00224541 -0.0736226 0.997284 2.48051 |
 | 0 0 0 1 |
 
 Init nav matrix with CAD correction
 | 0.060949 0.998118 -0.00674674 1.55312 |
 | -0.995422 0.0612801 0.0733472 -4.0873 |
 | 0.0736226 0.00224541 0.997284 -1.45508 |
 | 0 0 0 1 |
*/
