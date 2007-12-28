#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>

#include <osg/Geode>
#include <osg/PositionAttitudeTransform>
#include <osg/Version>
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>

#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
#include <osgProducer/Viewer>
#endif

#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/KnotVector.h>
#include <ves/xplorer/scenegraph/nurbs/NCurve.h>
#include <ves/xplorer/scenegraph/nurbs/NSurface.h>

#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#ifndef WIN32
#include <ves/xplorer/scenegraph/nurbs/util/OCCNURBSFileReader.h>
#ifdef HAS_OCC
#include <ves/xplorer/scenegraph/nurbs/util/IGES2VENURBS.h>
#endif
#endif
#include <ves/xplorer/util/fileIO.h>

void createTestNURBS(int argc, char** argv);
#ifndef WIN32
int parseOCCNURBSFile(int argc, char** argv);
#ifdef HAS_OCC
int parseIGESFile(int argc, char** argv);
#endif
#endif

class KeyboardEventHandler : public osgGA::GUIEventHandler
{
public:
    
   KeyboardEventHandler(std::vector< osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> > surfacePatches)
      :_isSelecting(false)
   {
      _patches.clear();
      for(size_t i = 0; i < surfacePatches.size(); i++)
      {
         _patches.push_back(surfacePatches.at(i));
      }
      _nPatches = _patches.size();
      _lastMousePosition[0] = 0;
      _lastMousePosition[1] = 0;
      _lastMousePosition[2] = 0;
   }
    
   virtual bool handle(const osgGA::GUIEventAdapter& ea,osgGA::GUIActionAdapter&)
   {
      if(_isSelecting)
      {
         switch(ea.getEventType())
         {
            case(osgGA::GUIEventAdapter::PUSH):
            {
               if((ea.getButtonMask()== osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON)||
                 (ea.getButtonMask()== osgGA::GUIEventAdapter::RIGHT_MOUSE_BUTTON))
               {
                  UpdateLastMousePosition(ea.getXnormalized(),ea.getYnormalized());
                  for(size_t i =0; i < _nPatches; i++)
                  {
                     //_patches.at(i)->SetMousePosition(ea.getXnormalized(),ea.getYnormalized());
                     //_patches.at(i)->SetSelectionStatus(true);
                  }
                  //to process drag events...
                  return false;
               }
               break;
            }
            case(osgGA::GUIEventAdapter::RELEASE):
            {
               //if(ea.getButtonMask()== osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON)
               {
                  //std::cout<<"Left Mouse release"<<std::endl;
                  for(size_t i =0; i < _nPatches; i++)
                  {
                     //_patches.at(i)->SetSelectionStatus(false);
                  }
               }
               UpdateLastMousePosition(ea.getXnormalized(),ea.getYnormalized());
               break;
            }
            case(osgGA::GUIEventAdapter::DRAG):
            {
               float dx = 0;
               float dy = 0;
               float dz = 0;
               if(ea.getButtonMask() == osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON)
               {
                  //x mouse move == X direction (left/right)
                  //y mouse move == Z direction (up/down)
                  dx = ea.getXnormalized() - _lastMousePosition[0];
                  dz = ea.getYnormalized() - _lastMousePosition[1];
                  if((fabs(dx) > .05)||(fabs(dz) > .05))
                  {
                     for(size_t i =0; i < _nPatches; i++)
                     {
                        //_patches.at(i)->MoveSelectedControlPoint(dx,0,dz);
                     }
                     UpdateLastMousePosition(ea.getXnormalized(),ea.getYnormalized());
                  }
               }
               else if(ea.getButtonMask() == osgGA::GUIEventAdapter::RIGHT_MOUSE_BUTTON)
               {
                  //y mouse move == zoom direction (in/out)
                  dy = ea.getYnormalized() - _lastMousePosition[2];
                  if((fabs(dy) > .05))
                  {
                     for(size_t i =0; i < _nPatches; i++)
                     {
                        //_patches.at(i)->MoveSelectedControlPoint(0,-dy,0);
                     }
                     UpdateLastMousePosition(ea.getXnormalized(),ea.getYnormalized());
                  }
               }
               break;
            }
            default:
               break;
         };
         
      }
      if((ea.getEventType() == osgGA::GUIEventAdapter::KEYDOWN) && 
            (ea.getKey() == 'P'))
      {
         _isSelecting = (!_isSelecting);
      }
     
      ///only need to check if we are turning on selection
      
      return _isSelecting;
   }

   ///Update the latest mouse position
   ///\param lastX The last x position
   ///\param lastY The last y position
   void UpdateLastMousePosition(float lastX,float lastY)
   {
      _lastMousePosition[0] = lastX;
      _lastMousePosition[1] = lastY;
      _lastMousePosition[2] = lastY;
   }
protected:
   bool _isSelecting;///< Indicates selection
   size_t _nPatches;///<The number of patches
   float _lastMousePosition[3];///<The change in mouse position.
   std::vector< osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> > _patches;///<The surface patches
        
};
/////////////render the NURBSurface in OSG
void render(int argc, char** argv,
            std::vector< osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> > surfacePatches)
{
   // use an ArgumentParser object to manage the program arguments.
    osg::ArgumentParser arguments(&argc,argv);

    // set up the usage document, in case we need to print out how to use this program.
    arguments.getApplicationUsage()->setDescription(arguments.getApplicationName()+" is the example which demonstrates both text, animation and billboard via custom transform to create the OpenSceneGraph logo..");
   
    arguments.getApplicationUsage()->setCommandLineUsage(arguments.getApplicationName()+"[options] [filename] ...");
    arguments.getApplicationUsage()->addCommandLineOption("-h or --help","Display this information");
    arguments.getApplicationUsage()->addCommandLineOption("ps","Render the Professional Services logo");
    KeyboardEventHandler* keh = new KeyboardEventHandler(surfacePatches);
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
    for(size_t i = 0; i < surfacePatches.size(); i++)
    {
        root->addChild(surfacePatches.at(i).get());
    }
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
    // construct the viewer.
    osgViewer::Viewer viewer(arguments);
    viewer.addEventHandler(keh);
    viewer.addEventHandler(new osgViewer::WindowSizeHandler);
    viewer.addEventHandler(new osgViewer::StatsHandler);
    viewer.setSceneData( root.get());
    viewer.realize();
    viewer.run();
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
    // construct the viewer.
    osgProducer::Viewer viewer(arguments);
    viewer.getEventHandlerList().push_front(keh);
    // set up the value with sensible default event handlers.
    viewer.setUpViewer(osgProducer::Viewer::STANDARD_SETTINGS);
    
    // get details on keyboard and mouse bindings used by the viewer.
    viewer.getUsage(*arguments.getApplicationUsage());
    while( !viewer.done() )
    {
        // wait for all cull and draw threads to complete.
        viewer.run();
        
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
#endif
    
}

int main(int argc, char** argv)
{
   if(argc >1)
   {
      std::string firstArg(argv[1]);
      if(std::string::npos != firstArg.find(".iges"))
      {
         std::cout<<"Found Iges file: "<<firstArg<<std::endl;  
#ifndef WIN32 
#ifdef HAS_OCC
         parseIGESFile(argc,argv);
#endif
#endif
         return 0;
      }
      else
      {
#ifndef WIN32
         return parseOCCNURBSFile(argc,argv);
#endif
      }
   }
   else
   {
      std::cout<<"Creating Test Scenario for NURBS"<<std::endl;
      createTestNURBS(argc,argv);
   }

   return 0;
}
///////////////////////////////////////
#ifndef WIN32
#ifdef HAS_OCC
int parseIGESFile(int argc,char** argv)
{

   std::string igesFile(argv[1]);
   std::vector< osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> >nurbsPatches;
   ves::xplorer::scenegraph::nurbs::util::IGES2VENURBS igesReader;
   std::vector<ves::xplorer::scenegraph::nurbs::NURBSSurface*> surfaces = igesReader.GetVectorOfVENURBSSurface(igesFile);
   size_t nPatches = surfaces.size();
   for(size_t i = 0; i < nPatches;i++)
   {
       //std::cout<<" patch:"<< i <<std::endl;
       surfaces.at(i)->SetInterpolationGridSize(10,"U");
       surfaces.at(i)->SetInterpolationGridSize(20,"V");
       //std::cout<<"patch info: "<<std::endl;
       //std::cout<<*surfaces.at(i)<<std::endl;
       //std::cout<<surfaces.at(i)->NumControlPoints("U")<<std::endl;
       //std::cout<<surfaces.at(i)->NumControlPoints("V")<<std::endl;
       //std::cout<<surfaces.at(i)->KnotVector("U").NumberOfKnots()<<std::endl;
       //std::cout<<surfaces.at(i)->KnotVector("V").NumberOfKnots()<<std::endl;
       //std::cout<<" interpolating patch:"<< i <<std::endl;
       surfaces.at(i)->Interpolate();
       //std::cout<<" Done:"<< i <<std::endl;

       osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> renderablePatch = new ves::xplorer::scenegraph::nurbs::NURBS(surfaces.at(i));
       nurbsPatches.push_back(renderablePatch.get());
   }
   if(nurbsPatches.size())
   {
      //std::cout<<"rendering patches"<<std::endl;
      render(argc,argv,nurbsPatches);
   }
   return 0;
}
#endif
////////////////////////////////////////////
int parseOCCNURBSFile(int argc, char** argv)
{
   std::vector< osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> >nurbsPatches;
   //std::string nurbsfile(argv[1]);
   std::vector< std::string > patchFiles = ves::xplorer::util::fileIO::GetFilesInDirectory(argv[1],".txt");
   size_t nPatches = patchFiles.size();
   ves::xplorer::scenegraph::nurbs::util::OCCNURBSFileReader patchReader;

   for(size_t i = 0; i < nPatches;i++)
   {
      ves::xplorer::scenegraph::nurbs::NURBSSurface* surface = patchReader.ReadPatchFile(patchFiles.at(i));
      if(surface)
      {
         surface->SetInterpolationGridSize(10,"U");
         surface->SetInterpolationGridSize(20,"V");
         surface->Interpolate();

         osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> renderablePatch = new ves::xplorer::scenegraph::nurbs::NURBS(surface);
         nurbsPatches.push_back(renderablePatch.get());
      }
      else
      {
         std::cout<<"Could not open file: "<<patchFiles.at(i)<<std::endl;
      }
   }
   if(nurbsPatches.size())
   {
      render(argc,argv,nurbsPatches);
   }
   return 0;
}
#endif
///////////////////////////////////////////
void createTestNURBS(int argc, char** argv)
{
   std::vector< osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> >testNURBSSurface;
   std::vector< osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> >testNURBSCurve;
   //Knot vector
   //U = {0,0,0,0,.5,1,1,1,1}
   ves::xplorer::scenegraph::nurbs::KnotVector knots;
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
   std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> controlPoints;
   controlPoints.push_back(ves::xplorer::scenegraph::nurbs::ControlPoint(0,0,0));   
   controlPoints.push_back(ves::xplorer::scenegraph::nurbs::ControlPoint(.25,1.0,0,2.0));
   controlPoints.push_back(ves::xplorer::scenegraph::nurbs::ControlPoint(.5,0.0,0,2.0));
   controlPoints.push_back(ves::xplorer::scenegraph::nurbs::ControlPoint(.75,1.0,0,1.0));
   controlPoints.push_back(ves::xplorer::scenegraph::nurbs::ControlPoint(1.0,0.0,0));

   //NURBSCurve
   ves::xplorer::scenegraph::nurbs::NURBSCurve ncurve(3);
   ncurve.SetControlPoints(controlPoints,controlPoints.size(),1);
   ncurve.SetKnotVector(knots);
   ncurve.SetInterpolationGridSize(30);
   ncurve.Interpolate();

   osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> renderableCurve = new ves::xplorer::scenegraph::nurbs::NURBS(&ncurve);
   testNURBSCurve.push_back(renderableCurve.get());
   //render(argc,argv,testNURBSCurve);

   float x = 0.0;
   float y = 0.0;
   float z = 0.0;

   std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> surfaceCtrlPts;
   for(unsigned int rows = 0; rows < 4; rows++)
   {
      for(unsigned int cols = 0; cols < 5; cols++)
      {
         surfaceCtrlPts.push_back(ves::xplorer::scenegraph::nurbs::ControlPoint(2.0*cols - 1.5,2.0*rows-1.5,3.0));
         if((rows == 0 || rows == 3) || (cols == 0 || cols == 4))
         {
            surfaceCtrlPts[rows*5 + cols].SetZ(-3.0);
         }
      }
   }
   surfaceCtrlPts.at(7).SetWeight(5);
   
   surfaceCtrlPts.at(11).SetWeight(3);
   ves::xplorer::scenegraph::nurbs::KnotVector uKnots;
   uKnots.AddKnot(0.0);
   uKnots.AddKnot(0.0);
   uKnots.AddKnot(0.0);
   uKnots.AddKnot(0.0);
   uKnots.AddKnot(0.5);

   uKnots.AddKnot(1.0);
   uKnots.AddKnot(1.0);
   uKnots.AddKnot(1.0);
   uKnots.AddKnot(1.0);

   ves::xplorer::scenegraph::nurbs::KnotVector vKnots;
   vKnots.AddKnot(0.0);
   vKnots.AddKnot(0.0);
   vKnots.AddKnot(0.0);
   vKnots.AddKnot(0.0);

   vKnots.AddKnot(1.0);
   vKnots.AddKnot(1.0);
   vKnots.AddKnot(1.0);
   vKnots.AddKnot(1.0);

   //the NURBS Surface (a patch)
   ves::xplorer::scenegraph::nurbs::NURBSSurface surface;
   surface.SetControlPoints(surfaceCtrlPts,5,4);
   surface.SetKnotVector(uKnots,"U");
   surface.SetKnotVector(vKnots,"V");
   surface.SetInterpolationGridSize(11,"U");
   surface.SetInterpolationGridSize(10,"V");
   surface.Interpolate();

   osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> renderablePatch = new ves::xplorer::scenegraph::nurbs::NURBS(&surface);
   testNURBSSurface.push_back(renderablePatch.get());

   //std::fstream fout("./testOut.ven",std::ios::out);
   render(argc,argv,testNURBSSurface);
   //fout<<surface<<std::endl;
   //ves::xplorer::scenegraph::nurbs::NURBSRenderer osgSurface(&surface);
   //osgSurface.ViewWireframe(true);
   //render(argc,argv,osgSurface);
}

