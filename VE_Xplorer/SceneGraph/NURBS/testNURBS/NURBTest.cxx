#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>

#include <osg/Geode>
#include <osg/PositionAttitudeTransform>

#include <osgProducer/Viewer>

#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"
#include "VE_Xplorer/SceneGraph/NURBS/KnotVector.h"
#include "VE_Xplorer/SceneGraph/NURBS/NCurve.h"
#include "VE_Xplorer/SceneGraph/NURBS/NSurface.h"
#include "VE_Xplorer/SceneGraph/NURBS/NURBSRenderer.h"
#include "VE_Xplorer/SceneGraph/NURBS/NURBSNode.h"

void createTestNURBS(int argc, char** argv);
int parseOCCNURBSFile(int argc, char** argv);

/////////////render the NURBSurface in OSG
void render(int argc, char** argv,NURBS::NURBSRenderer surface)
{
   // use an ArgumentParser object to manage the program arguments.
    osg::ArgumentParser arguments(&argc,argv);

    // set up the usage document, in case we need to print out how to use this program.
    arguments.getApplicationUsage()->setDescription(arguments.getApplicationName()+" is the example which demonstrates both text, animation and billboard via custom transform to create the OpenSceneGraph logo..");
   
    arguments.getApplicationUsage()->setCommandLineUsage(arguments.getApplicationName()+"[options] [filename] ...");
    arguments.getApplicationUsage()->addCommandLineOption("-h or --help","Display this information");
    arguments.getApplicationUsage()->addCommandLineOption("ps","Render the Professional Services logo");
   
    // construct the viewer.
    osgProducer::Viewer viewer(arguments);

    // set up the value with sensible default event handlers.
    viewer.setUpViewer(osgProducer::Viewer::STANDARD_SETTINGS);

    // get details on keyboard and mouse bindings used by the viewer.
    viewer.getUsage(*arguments.getApplicationUsage());

    // if user request help write it out to cout.
    if (arguments.read("-h") || arguments.read("--help"))
    {
        arguments.getApplicationUsage()->write(std::cout);
        return ;
    }
    
    // any option left unread are converted into errors to write out later.
    arguments.reportRemainingOptionsAsUnrecognized();

    // report any errors if they have occured when parsing the program aguments.
    if (arguments.errors())
    {
        arguments.writeErrorMessages(std::cout);
        return ;
    }
    osg::ref_ptr<osg::PositionAttitudeTransform> root = new osg::PositionAttitudeTransform();
    //root->addChild(surface.GetTriangulatedSurface());
    osg::ref_ptr<NURBS::NURBSNode> nurbsNodeTest = new NURBS::NURBSNode(surface.GetNURBS());
    root->addChild(nurbsNodeTest.get());
    //root->addChild(surface.GetControlMesh());

    // add model to viewer.
    viewer.setSceneData( root.get());

    // create the windows and run the threads.
    viewer.realize();

    while( !viewer.done() )
    {
        // wait for all cull and draw threads to complete.
        viewer.sync();

        // update the scene by traversing it with the the update visitor which will
        // call all node update callbacks and animations.
        viewer.update();
         
        // fire off the cull and draw traversals of the scene.
        viewer.frame();
        
    }
    
    // wait for all cull and draw threads to complete.
    viewer.sync();

    // run a clean up frame to delete all OpenGL objects.
    viewer.cleanup_frame();

    // wait for all the clean up frame to complete.
    viewer.sync();
}

int main(int argc, char** argv)
{
   

   if(argc >1)
   {
      return parseOCCNURBSFile(argc,argv);
   }
   else
   {
      createTestNURBS(argc,argv);
   }

   return 0;
}
////////////////////////////////////////////
int parseOCCNURBSFile(int argc, char** argv)
{
   std::string nurbsfile(argv[1]);
   std::fstream occNURBSFile(nurbsfile.c_str(),std::ios::in);
   if(occNURBSFile.is_open())
   {
      char descriptorLine[ 2048 ];
      std::cout<<"Opened file: "<<nurbsfile<<std::endl;
      
      //U knots descriptor
      occNURBSFile.getline(descriptorLine,2048);

      //U knot values
      occNURBSFile.getline(descriptorLine,2048);

      std::istringstream strm( descriptorLine );

      double knot = 0;
      NURBS::KnotVector uKnots;      
   
      while(strm >> knot)
      {
         uKnots.AddKnot(knot);
      }
      //V knots descriptor
      occNURBSFile.getline(descriptorLine,2048);

      //V knot values
      occNURBSFile.getline(descriptorLine,2048);
      strm.clear();
      strm.str(descriptorLine);
      NURBS::KnotVector vKnots;
      while(strm >> knot)
      {
         vKnots.AddKnot(knot);
      }
        
      std::vector<NURBS::ControlPoint> surfaceCtrlPts;
      double x = 0;
      double y = 0;
      double z = 0;
      double w = 0;
      //Control points descriptor
      occNURBSFile.getline(descriptorLine,2048);

      //control points
      char delimChar[256];
      unsigned int nU = 0;
      unsigned int nV = 0;
      while(occNURBSFile.getline(descriptorLine,2048))
      {
         nV = 0;
         strm.clear();
         strm.str(descriptorLine);
         while(strm >> delimChar)
         {
            strm>>x>>y>>z>>w;
            surfaceCtrlPts.push_back(NURBS::ControlPoint(x,y,z,w));
            nV++;
            //")"
            strm>>delimChar;
         }
         nU++;
      }

      //the NURBS Surface (a patch)
      NURBS::NURBSSurface surface;
      surface.SetControlPoints(surfaceCtrlPts,nU,nV);
      surface.SetKnotVector(uKnots,"U");
      surface.SetKnotVector(vKnots,"V");
      surface.SetInterpolationGridSize(5,"U");
      surface.SetInterpolationGridSize(5,"V");
      surface.Interpolate();
   
      NURBS::NURBSRenderer osgSurface(&surface);
      //osgSurface.ViewWireframe(true);
      render(argc,argv,osgSurface);
      return 0;
   }
   else
   {
      std::cout<<"Could not open file: "<<nurbsfile<<std::endl;
      return -1;
   }
}
///////////////////////////////////////////
void createTestNURBS(int argc, char** argv)
{
   //Knot vector
   //U = {0,0,0,0,.5,1,1,1,1}
   NURBS::KnotVector knots;
   knots.AddKnot(0.0);
   knots.AddKnot(0.0);
   knots.AddKnot(0.0);
   knots.AddKnot(0.0);
   knots.AddKnot(0.5);

   knots.AddKnot(1.0);
   knots.AddKnot(1.0);
   knots.AddKnot(1.0);
   knots.AddKnot(1.0);

   //Control points
   std::vector<NURBS::ControlPoint> controlPoints;
   controlPoints.push_back(NURBS::ControlPoint(0,0,0));   
   controlPoints.push_back(NURBS::ControlPoint(.25,1.0,0,2.0));
   controlPoints.push_back(NURBS::ControlPoint(.5,0.0,0,2.0));
   controlPoints.push_back(NURBS::ControlPoint(.75,1.0,0,1.0));
   controlPoints.push_back(NURBS::ControlPoint(1.0,0.0,0));

   //NURBSCurve
   NURBS::NURBSCurve ncurve(3);
   ncurve.SetControlPoints(controlPoints,controlPoints.size());
   ncurve.SetKnotVector(knots);
   ncurve.SetInterpolationGridSize(30);
   ncurve.Interpolate();

   float x = 0.0;
   float y = 0.0;
   float z = 0.0;

   std::vector<NURBS::ControlPoint> surfaceCtrlPts;
   for(unsigned int rows = 0; rows < 4; rows++)
   {

      for(unsigned int cols = 0; cols < 5; cols++)
      {
         surfaceCtrlPts.push_back(NURBS::ControlPoint(2.0*rows - 1.5,2.0*cols-1.5,3.0,.5));
         if((rows == 0 || rows == 3) || (cols == 0 || cols == 4))
         {
            surfaceCtrlPts[rows*5 + cols].SetZ(-3.0);
         }
      }
   }
   surfaceCtrlPts.at(7).SetWeight(10.0);
   NURBS::KnotVector uKnots;
   uKnots.AddKnot(0.0);
   uKnots.AddKnot(0.0);
   uKnots.AddKnot(0.0);
   uKnots.AddKnot(0.0);

   uKnots.AddKnot(1.0);
   uKnots.AddKnot(1.0);
   uKnots.AddKnot(1.0);
   uKnots.AddKnot(1.0);

   NURBS::KnotVector vKnots;
   vKnots.AddKnot(0.0);
   vKnots.AddKnot(0.0);
   vKnots.AddKnot(0.0);
   vKnots.AddKnot(0.0);
   vKnots.AddKnot(0.5);

   vKnots.AddKnot(1.0);
   vKnots.AddKnot(1.0);
   vKnots.AddKnot(1.0);
   vKnots.AddKnot(1.0);

   //the NURBS Surface (a patch)
   NURBS::NURBSSurface surface;
   surface.SetControlPoints(surfaceCtrlPts,4,5);
   surface.SetKnotVector(uKnots,"U");
   surface.SetKnotVector(vKnots,"V");
   surface.SetInterpolationGridSize(10,"U");
   surface.SetInterpolationGridSize(10,"V");
   surface.Interpolate();

   /*std::fstream fout3("./testSurfacePoints.txt",std::ios::out);
   for(size_t i = 0; i < surface.InterpolatedPoints().size(); i++)
   {
      fout3<<surface.InterpolatedPoints().at(i)<<std::endl;
   }

   std::fstream fout4("./surfaceontrolPoints.txt",std::ios::out);
   for(size_t i = 0; i < surface.ControlPoints().size(); i++)
   {
      fout4<<surface.GetControlPoint(i)<<std::endl;
   }*/

   NURBS::NURBSRenderer osgCurve(&ncurve);
   render(argc,argv,osgCurve);

   NURBS::NURBSRenderer osgSurface(&surface);
   //osgSurface.ViewWireframe(true);
   render(argc,argv,osgSurface);
}

