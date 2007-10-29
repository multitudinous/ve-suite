BuildRequires: aaa_base acl attr bash binutils bison bzip2 coreutils cpio cpp devs diffutils doxygen e2fsprogs expat file filesystem findutils freetype2 freetype2-devel gawk gcc gcc-c++ giflib giflib-devel glibc glibc-devel glibc-locale grep gzip info klogd libacl libattr libgcc libjpeg libjpeg-devel libpng libpng-devel libstdc++ libstdc++-devel libtiff libtiff-devel m4 make man mktemp module-init-tools patch permissions popt procinfo procps psmisc pwdutils readline sed sysvinit tar timezone unzip util-linux xorg-x11-Mesa xorg-x11-Mesa-devel xorg-x11-devel xorg-x11-libs zlib zlib-devel OpenProducer OpenProducer-devel openthreads openthreads-devel

Name: OpenSceneGraph
Summary: A C++ scene graph API on OpenGL for real time graphics
Version: 1.2.0
Release: 1
License: LGPL
Group: Graphics
Source0: %{name}-%{version}.tar.bz2
URL: http://www.openscenegraph.org

%description

The Open Scene Graph is a cross-platform C++/OpenGL library for the real-time
visualization. Uses range from visual simulation, scientific modeling, virtual
reality through to games.  Open Scene Graph employs good practices in software
engineering through the use of standard C++, STL and generic programming, and
design patterns.  Open Scene Graph strives for high performance and quality in
graphics rendering, protability, and extensibility.

Authors:
--------
   Sean Spicer
   Jack Lees
   Don Burns
   Robert Osfield
   Boris Bralo

%package devel
Summary: Development headers for OpenSceneGraph
Version: 1.2.0
Release: 1
License: LGPL
Group: Development/C++

%description devel

This package contains the development headers necessary to compile applications
written using OpenSceneGraph.

Authors:
--------
   Sean Spicer
   Jack Lees
   Don Burns
   Robert Osfield
   Boris Bralo

#%package examples
#Summary: OpenSceneGraph example programs 
#Version: 1.2.0
#Release: 1
#License: LGPL
#Group: Development/C++
#
#%description examples
#
#This package contains several examples of programs that use OpenSceneGraph.
#
#Authors:
#--------
#   Sean Spicer
#   Jack Lees
#   Don Burns
#   Robert Osfield
#   Boris Bralo
#
%prep

%setup

%build

make INST_LOCATION=%{_prefix}

%install

make DESTDIR=$RPM_BUILD_ROOT INST_LOCATION=%{_prefix} install

# Install the pkg-config file as well.
touch %{name}.pc.paths
echo "# %{name} pkg-config file" >> %{name}.pc.paths
echo "prefix=%{_prefix}" >> %{name}.pc.paths
echo "exec_prefix=%{_exec_prefix}" >> %{name}.pc.paths
echo "libdir=%{_libdir}" >> %{name}.pc.paths echo "includedir=%{_includedir}" >> %{name}.pc.paths
cat %{name}.pc.paths openscenegraph.pc.rpmin > openscenegraph.pc
cp openscenegraph.pc %{_prefix}/lib/pkgconfig/openscenegraph.pc

%post
%run_ldconfig

%postun
%run_ldconfig

%files
%defattr(755, root, root)
%dir %{_prefix}/share/OpenSceneGraph
%dir %{_prefix}/share/OpenSceneGraph/bin
%dir %{_libdir}/osgPlugins
%{_prefix}/share/OpenSceneGraph/bin/osgarchive
%{_prefix}/share/OpenSceneGraph/bin/osgconv
%{_prefix}/share/OpenSceneGraph/bin/osgversion
%{_prefix}/share/OpenSceneGraph/bin/osgviewer
%{_libdir}/libosg.so
%{_libdir}/libosgText.so
%{_libdir}/libosgParticle.so
%{_libdir}/osgPlugins/osgdb_bmp.so
%{_libdir}/osgPlugins/osgdb_bsp.so
%{_libdir}/osgPlugins/osgdb_dds.so
%{_libdir}/osgPlugins/osgdb_dxf.so
%{_libdir}/osgPlugins/osgdb_flt.so
%{_libdir}/osgPlugins/osgdb_geo.so
%{_libdir}/osgPlugins/osgdb_gif.so
%{_libdir}/osgPlugins/osgdb_freetype.so
%{_libdir}/osgPlugins/osgdb_hdr.so
%{_libdir}/osgPlugins/osgdb_ive.so
%{_libdir}/osgPlugins/osgdb_md2.so
%{_libdir}/osgPlugins/osgdb_lwo.so
%{_libdir}/osgPlugins/osgdb_lws.so
%{_libdir}/osgPlugins/osgdb_net.so
%{_libdir}/osgPlugins/osgdb_obj.so
%{_libdir}/osgPlugins/osgdb_osg.so
%{_libdir}/osgPlugins/osgdb_pic.so
%{_libdir}/osgPlugins/osgdb_png.so
%{_libdir}/osgPlugins/osgdb_pnm.so
%{_libdir}/osgPlugins/osgdb_rgb.so
%{_libdir}/osgPlugins/osgdb_jpeg.so
%{_libdir}/osgPlugins/osgdb_x.so
%{_libdir}/osgPlugins/osgdb_rot.so
%{_libdir}/osgPlugins/osgdb_shp.so
%{_libdir}/osgPlugins/osgdb_tga.so
%{_libdir}/osgPlugins/osgdb_tgz.so
%{_libdir}/osgPlugins/osgdb_stl.so
%{_libdir}/osgPlugins/osgdb_osgText.so
%{_libdir}/osgPlugins/osgdb_txp.so
%{_libdir}/osgPlugins/osgdb_ac.so
%{_libdir}/osgPlugins/osgdb_dw.so
%{_libdir}/osgPlugins/osgdb_zip.so
%{_libdir}/osgPlugins/osgdb_osgtgz.so
%{_libdir}/osgPlugins/osgdb_3dc.so
%{_libdir}/osgPlugins/osgdb_3ds.so
%{_libdir}/osgPlugins/osgdb_OpenFlight.so
%{_libdir}/osgPlugins/osgdb_logo.so
%{_libdir}/osgPlugins/osgdb_tiff.so
%{_libdir}/osgPlugins/osgdb_trans.so
%{_libdir}/osgPlugins/osgdb_osgFX.so
%{_libdir}/osgPlugins/osgdb_scale.so
%{_libdir}/osgPlugins/osgdb_normals.so
%{_libdir}/osgPlugins/osgdb_osgParticle.so
%{_libdir}/osgPlugins/osgdb_osgSim.so
%{_libdir}/osgPlugins/osgdb_osga.so
%{_libdir}/libosgUtil.so
%{_libdir}/libosgSim.so
%{_libdir}/libosgDB.so
%{_libdir}/libosgGA.so
%{_libdir}/libosgFX.so
%{_libdir}/libosgProducer.so

%files devel
%defattr(755, root, root)
%dir %{_includedir}/osg
%dir %{_includedir}/osgIntrospection
%dir %{_includedir}/osgDB
%dir %{_includedir}/osgGA
%dir %{_includedir}/osgFX
%dir %{_includedir}/osgParticle
%dir %{_includedir}/osgSim
%dir %{_includedir}/osgProducer
%dir %{_includedir}/osgText
%dir %{_includedir}/osgUtil
%dir %{_includedir}/osgTerrain
%defattr(644, root, root)
%{_includedir}/osg/GL
%{_includedir}/osg/GLU
%{_includedir}/osg/Fog
%{_includedir}/osg/LOD
%{_includedir}/osg/Math
%{_includedir}/osg/Node
%{_includedir}/osg/Quat
%{_includedir}/osg/Vec2
%{_includedir}/osg/Vec3
%{_includedir}/osg/Vec4
%{_includedir}/osg/Version
%{_includedir}/osg/CullFace
%{_includedir}/osg/Polytope
%{_includedir}/osg/BlendColor
%{_includedir}/osg/FrameStamp
%{_includedir}/osg/CopyOp
%{_includedir}/osg/LightSource
%{_includedir}/osg/TriangleIndexFunctor
%{_includedir}/osg/Drawable
%{_includedir}/osg/Endian
%{_includedir}/osg/PointSprite
%{_includedir}/osg/Export
%{_includedir}/osg/ref_ptr
%{_includedir}/osg/Array
%{_includedir}/osg/Depth
%{_includedir}/osg/NodeCallback
%{_includedir}/osg/Geode
%{_includedir}/osg/Group
%{_includedir}/osg/Image
%{_includedir}/osg/Light
%{_includedir}/osg/Plane
%{_includedir}/osg/Point
%{_includedir}/osg/CollectOccludersVisitor
%{_includedir}/osg/Shape
%{_includedir}/osg/State
%{_includedir}/osg/Timer
%{_includedir}/osg/Vec2b
%{_includedir}/osg/Vec2d
%{_includedir}/osg/Vec2f
%{_includedir}/osg/Vec3b
%{_includedir}/osg/Vec3d
%{_includedir}/osg/Vec3f
%{_includedir}/osg/Vec2s
%{_includedir}/osg/Vec4b
%{_includedir}/osg/Vec4d
%{_includedir}/osg/Vec4f
%{_includedir}/osg/Vec3s
%{_includedir}/osg/Vec4s
%{_includedir}/osg/GraphicsThread
%{_includedir}/osg/Geometry
%{_includedir}/osg/CoordinateSystemNode
%{_includedir}/osg/TexEnvCombine
%{_includedir}/osg/Matrix
%{_includedir}/osg/VertexProgram
%{_includedir}/osg/Object
%{_includedir}/osg/Notify
%{_includedir}/osg/Material
%{_includedir}/osg/AlphaFunc
%{_includedir}/osg/Shader
%{_includedir}/osg/BoundsChecking
%{_includedir}/osg/TexEnv
%{_includedir}/osg/TexGen
%{_includedir}/osg/TexMat
%{_includedir}/osg/ShadeModel
%{_includedir}/osg/Switch
%{_includedir}/osg/PrimitiveSet
%{_includedir}/osg/Vec4ub
%{_includedir}/osg/BlendFunc
%{_includedir}/osg/observer_ptr
%{_includedir}/osg/CullStack
%{_includedir}/osg/Texture1D
%{_includedir}/osg/Texture2D
%{_includedir}/osg/Texture3D
%{_includedir}/osg/Program
%{_includedir}/osg/PolygonOffset
%{_includedir}/osg/PolygonStipple
%{_includedir}/osg/LineWidth
%{_includedir}/osg/LightModel
%{_includedir}/osg/fast_back_stack
%{_includedir}/osg/StateAttribute
%{_includedir}/osg/StateSet
%{_includedir}/osg/Sequence
%{_includedir}/osg/ClipPlane
%{_includedir}/osg/TexGenNode
%{_includedir}/osg/NodeVisitor
%{_includedir}/osg/CullSettings
%{_includedir}/osg/buffered_value
%{_includedir}/osg/Billboard
%{_includedir}/osg/CullingSet
%{_includedir}/osg/ColorMatrix
%{_includedir}/osg/Projection
%{_includedir}/osg/Scissor
%{_includedir}/osg/AutoTransform
%{_includedir}/osg/TextureCubeMap
%{_includedir}/osg/BlendEquation
%{_includedir}/osg/OccluderNode
%{_includedir}/osg/DisplaySettings
%{_includedir}/osg/ProxyNode
%{_includedir}/osg/Referenced
%{_includedir}/osg/ImageStream
%{_includedir}/osg/Viewport
%{_includedir}/osg/LineSegment
%{_includedir}/osg/ApplicationUsage
%{_includedir}/osg/Matrixd
%{_includedir}/osg/Matrixf
%{_includedir}/osg/ArgumentParser
%{_includedir}/osg/BufferObject
%{_includedir}/osg/TriangleFunctor
%{_includedir}/osg/ClipNode
%{_includedir}/osg/LogicOp
%{_includedir}/osg/ColorMask
%{_includedir}/osg/AnimationPath
%{_includedir}/osg/ClearNode
%{_includedir}/osg/FrontFace
%{_includedir}/osg/UnitTestFramework
%{_includedir}/osg/CameraNode
%{_includedir}/osg/CameraView
%{_includedir}/osg/Transform
%{_includedir}/osg/Texture
%{_includedir}/osg/DrawPixels
%{_includedir}/osg/FragmentProgram
%{_includedir}/osg/BoundingBox
%{_includedir}/osg/Multisample
%{_includedir}/osg/GL2Extensions
%{_includedir}/osg/ShapeDrawable
%{_includedir}/osg/Stencil
%{_includedir}/osg/TextureRectangle
%{_includedir}/osg/LineStipple
%{_includedir}/osg/io_utils
%{_includedir}/osg/ConvexPlanarOccluder
%{_includedir}/osg/GraphicsContext
%{_includedir}/osg/PagedLOD
%{_includedir}/osg/ClampColor
%{_includedir}/osg/PolygonMode
%{_includedir}/osg/ShadowVolumeOccluder
%{_includedir}/osg/NodeTrackerCallback
%{_includedir}/osg/ConvexPlanarPolygon
%{_includedir}/osg/TexEnvFilter
%{_includedir}/osg/PositionAttitudeTransform
%{_includedir}/osg/GLExtensions
%{_includedir}/osg/BoundingSphere
%{_includedir}/osg/Uniform
%{_includedir}/osg/ClusterCullingCallback
%{_includedir}/osg/FrameBufferObject
%{_includedir}/osg/MatrixTransform
%{_includedir}/osgIntrospection/Type
%{_includedir}/osgIntrospection/ReaderWriter
%{_includedir}/osgIntrospection/PropertyInfo
%{_includedir}/osgIntrospection/Converter
%{_includedir}/osgIntrospection/Utility
%{_includedir}/osgIntrospection/Export
%{_includedir}/osgIntrospection/Comparator
%{_includedir}/osgIntrospection/Value
%{_includedir}/osgIntrospection/StaticMethodInfo
%{_includedir}/osgIntrospection/ReflectionMacros
%{_includedir}/osgIntrospection/MethodInfo
%{_includedir}/osgIntrospection/PublicMemberAccessor
%{_includedir}/osgIntrospection/Attributes
%{_includedir}/osgIntrospection/Reflector
%{_includedir}/osgIntrospection/Exceptions
%{_includedir}/osgIntrospection/ConverterProxy
%{_includedir}/osgIntrospection/TypeNameAliasProxy
%{_includedir}/osgIntrospection/Reflection
%{_includedir}/osgIntrospection/variant_cast
%{_includedir}/osgIntrospection/ParameterInfo
%{_includedir}/osgIntrospection/ConstructorInfo
%{_includedir}/osgIntrospection/TypedConstructorInfo
%{_includedir}/osgIntrospection/CustomAttributeProvider
%{_includedir}/osgIntrospection/CustomAttribute
%{_includedir}/osgIntrospection/TypedMethodInfo
%{_includedir}/osgIntrospection/InstanceCreator
%{_includedir}/osgDB/ReaderWriter
%{_includedir}/osgDB/DynamicLibrary
%{_includedir}/osgDB/Version
%{_includedir}/osgDB/Export
%{_includedir}/osgDB/ParameterOutput
%{_includedir}/osgDB/Field
%{_includedir}/osgDB/Input
%{_includedir}/osgDB/Archive
%{_includedir}/osgDB/SharedStateManager
%{_includedir}/osgDB/Output
%{_includedir}/osgDB/FieldReaderIterator
%{_includedir}/osgDB/DatabasePager
%{_includedir}/osgDB/ImageOptions
%{_includedir}/osgDB/FieldReader
%{_includedir}/osgDB/FileNameUtils
%{_includedir}/osgDB/ReadFile
%{_includedir}/osgDB/FileUtils
%{_includedir}/osgDB/WriteFile
%{_includedir}/osgDB/Registry
%{_includedir}/osgDB/DotOsgWrapper
%{_includedir}/osgDB/ReentrantMutex
%{_includedir}/osgGA/UFOManipulator
%{_includedir}/osgGA/Version
%{_includedir}/osgGA/Export
%{_includedir}/osgGA/AnimationPathManipulator
%{_includedir}/osgGA/GUIEventHandlerVisitor
%{_includedir}/osgGA/TrackballManipulator
%{_includedir}/osgGA/EventVisitor
%{_includedir}/osgGA/GUIEventHandler
%{_includedir}/osgGA/GUIEventAdapter
%{_includedir}/osgGA/NodeTrackerManipulator
%{_includedir}/osgGA/GUIActionAdapter
%{_includedir}/osgGA/FlightManipulator
%{_includedir}/osgGA/TerrainManipulator
%{_includedir}/osgGA/MatrixManipulator
%{_includedir}/osgGA/StateSetManipulator
%{_includedir}/osgGA/EventQueue
%{_includedir}/osgGA/KeySwitchMatrixManipulator
%{_includedir}/osgGA/DriveManipulator
%{_includedir}/osgGA/SetSceneViewVisitor
%{_includedir}/osgFX/Effect
%{_includedir}/osgFX/Technique
%{_includedir}/osgFX/Export
%{_includedir}/osgFX/Validator
%{_includedir}/osgFX/Cartoon
%{_includedir}/osgFX/SpecularHighlights
%{_includedir}/osgFX/Scribe
%{_includedir}/osgFX/AnisotropicLighting
%{_includedir}/osgFX/MultiTextureControl
%{_includedir}/osgFX/BumpMapping
%{_includedir}/osgFX/Registry
%{_includedir}/osgParticle/SmokeEffect
%{_includedir}/osgParticle/PointPlacer
%{_includedir}/osgParticle/Version
%{_includedir}/osgParticle/ExplosionDebrisEffect
%{_includedir}/osgParticle/ParticleSystemUpdater
%{_includedir}/osgParticle/FluidProgram
%{_includedir}/osgParticle/Particle
%{_includedir}/osgParticle/Export
%{_includedir}/osgParticle/ModularEmitter
%{_includedir}/osgParticle/ExplosionEffect
%{_includedir}/osgParticle/range
%{_includedir}/osgParticle/Placer
%{_includedir}/osgParticle/VariableRateCounter
%{_includedir}/osgParticle/PrecipitationEffect
%{_includedir}/osgParticle/ParticleProcessor
%{_includedir}/osgParticle/Program
%{_includedir}/osgParticle/SegmentPlacer
%{_includedir}/osgParticle/Operator
%{_includedir}/osgParticle/Counter
%{_includedir}/osgParticle/Interpolator
%{_includedir}/osgParticle/BoxPlacer
%{_includedir}/osgParticle/RadialShooter
%{_includedir}/osgParticle/FireEffect
%{_includedir}/osgParticle/ConnectedParticleSystem
%{_includedir}/osgParticle/Shooter
%{_includedir}/osgParticle/Emitter
%{_includedir}/osgParticle/SmokeTrailEffect
%{_includedir}/osgParticle/SectorPlacer
%{_includedir}/osgParticle/ConstantRateCounter
%{_includedir}/osgParticle/AngularAccelOperator
%{_includedir}/osgParticle/ParticleEffect
%{_includedir}/osgParticle/ModularProgram
%{_includedir}/osgParticle/AccelOperator
%{_includedir}/osgParticle/ForceOperator
%{_includedir}/osgParticle/FluidFrictionOperator
%{_includedir}/osgParticle/CenteredPlacer
%{_includedir}/osgParticle/MultiSegmentPlacer
%{_includedir}/osgParticle/ParticleSystem
%{_includedir}/osgParticle/LinearInterpolator
%{_includedir}/osgParticle/RandomRateCounter
%{_includedir}/osgSim/Version
%{_includedir}/osgSim/ScalarBar
%{_includedir}/osgSim/Export
%{_includedir}/osgSim/GeographicLocation
%{_includedir}/osgSim/BlinkSequence
%{_includedir}/osgSim/ImpostorSprite
%{_includedir}/osgSim/ScalarsToColors
%{_includedir}/osgSim/Sector
%{_includedir}/osgSim/MultiSwitch
%{_includedir}/osgSim/SphereSegment
%{_includedir}/osgSim/LightPoint
%{_includedir}/osgSim/LightPointNode
%{_includedir}/osgSim/Impostor
%{_includedir}/osgSim/ColorRange
%{_includedir}/osgSim/OverlayNode
%{_includedir}/osgSim/VisibilityGroup
%{_includedir}/osgSim/DOFTransform
%{_includedir}/osgSim/InsertImpostorsVisitor
%{_includedir}/osgSim/OpenFlightOptimizer
%{_includedir}/osgSim/LightPointSystem
%{_includedir}/osgProducer/Version
%{_includedir}/osgProducer/Export
%{_includedir}/osgProducer/ViewerEventHandler
%{_includedir}/osgProducer/Viewer
%{_includedir}/osgProducer/KeyboardMouseCallback
%{_includedir}/osgProducer/GraphicsContextImplementation
%{_includedir}/osgProducer/OsgSceneHandler
%{_includedir}/osgProducer/OsgCameraGroup
%{_includedir}/osgText/Font
%{_includedir}/osgText/Text
%{_includedir}/osgText/Version
%{_includedir}/osgText/Export
%{_includedir}/osgText/String
%{_includedir}/osgUtil/Version
%{_includedir}/osgUtil/Export
%{_includedir}/osgUtil/RenderLeaf
%{_includedir}/osgUtil/RenderStage
%{_includedir}/osgUtil/TransformAttributeFunctor
%{_includedir}/osgUtil/IntersectVisitor
%{_includedir}/osgUtil/HalfWayMapGenerator
%{_includedir}/osgUtil/GLObjectsVisitor
%{_includedir}/osgUtil/Optimizer
%{_includedir}/osgUtil/DelaunayTriangulator
%{_includedir}/osgUtil/TangentSpaceGenerator
%{_includedir}/osgUtil/DisplayRequirementsVisitor
%{_includedir}/osgUtil/RenderBin
%{_includedir}/osgUtil/StateGraph
%{_includedir}/osgUtil/UpdateVisitor
%{_includedir}/osgUtil/TransformCallback
%{_includedir}/osgUtil/HighlightMapGenerator
%{_includedir}/osgUtil/SmoothingVisitor
%{_includedir}/osgUtil/Statistics
%{_includedir}/osgUtil/CullVisitor
%{_includedir}/osgUtil/SceneView
%{_includedir}/osgUtil/CubeMapGenerator
%{_includedir}/osgUtil/Simplifier
%{_includedir}/osgUtil/Tesselator
%{_includedir}/osgUtil/TriStripVisitor
%{_includedir}/osgUtil/ReflectionMapGenerator
%{_includedir}/osgUtil/PositionalStateContainer
%{_includedir}/osgTerrain/Version
%{_includedir}/osgTerrain/Export
%{_includedir}/osgTerrain/HeightFieldNode
%{_includedir}/osgTerrain/DataSet
%{_includedir}/osgTerrain/HeightFieldRenderer

#%files examples
#%defattr(644, root, root)
#%dir %{_prefix}/share/OpenSceneGraph/src
#%dir %{_prefix}/share/OpenSceneGraph/src/Make
#%dir %{_prefix}/share/OpenSceneGraph/src/examples
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgsimulation
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgGLUTsimple
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgsequence
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgfxbrowser
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgoccluder
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgbillboard
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgwindows
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgmultiplecameras
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgplanets
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgshadowtexture
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgimpostor
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgprerendercubemap
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgsimplepager
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgparticleeffects
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgtesselate
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgcatch
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgcegui
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osglight
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgmovie
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgmultitexture
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgshape
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgslice
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgpointsprite
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osglauncher
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgcluster
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgkeyboard
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgsimplifier
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgreflect
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgcubemap
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgkeyboardmouse
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgspacewarp
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgGLUTkeyboardmouse
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgshaderterrain
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgmotionblur
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgvertexprogram
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgcamera
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgshaders
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgpagedlod
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgblendequation
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgforest
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgautotransform
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osglogicop
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgprecipitation
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgcameragroup
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgtexturerectangle
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osghud
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgunittests
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgscalarbar
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgspheresegment
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgspotlight
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgpoints
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgprerender
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgparticle
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgscribe
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgsimple
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgstereoimage
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgteapot
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osggeodemo
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgcallback
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osggeometry
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgvolume
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgphotoalbum
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgdistortion
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osglightpoint
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgclip
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgcopy
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osglogo
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgpick
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgtext
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgintrospection
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgdepthpartition
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgtexture1D
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgtexture2D
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgtexture3D
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgdelaunay
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgparametric
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osghangglide
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osganimate
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgbluemarble
#%dir %{_prefix}/share/OpenSceneGraph/src/examples/osgdepthshadow
#
#%{_prefix}/share/OpenSceneGraph/src/Make/makerules
#%{_prefix}/share/OpenSceneGraph/src/Make/makedefs
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsimulation/osgsimulation.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsimulation/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgGLUTsimple/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgGLUTsimple/osgGLUTsimple.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsequence/osgsequence.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsequence/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgfxbrowser/osgfxbrowser.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgfxbrowser/Frame.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgfxbrowser/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgoccluder/osgoccluder.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgoccluder/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgbillboard/osgbillboard.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgbillboard/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgwindows/osgwindows.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgwindows/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgmultiplecameras/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgmultiplecameras/osgmultiplecameras.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgplanets/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgplanets/osgplanets.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshadowtexture/CreateShadowedScene.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshadowtexture/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshadowtexture/osgshadowtexture.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgimpostor/osgimpostor.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgimpostor/TestManipulator.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgimpostor/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgprerendercubemap/osgprerendercubemap.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgprerendercubemap/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsimplepager/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsimplepager/osgsimplepager.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgparticleeffects/osgparticleeffects.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgparticleeffects/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtesselate/osgtesselate.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtesselate/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcatch/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcatch/osgcatch.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcegui/osgcegui.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcegui/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osglight/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osglight/osglight.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgmovie/osgmovie.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgmovie/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgmultitexture/osgmultitexture.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgmultitexture/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshape/osgshape.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshape/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgslice/osgslice.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgslice/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgpointsprite/osgpointsprite.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgpointsprite/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osglauncher/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osglauncher/osglauncher.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcluster/broadcaster.h
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcluster/broadcaster.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcluster/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcluster/receiver.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcluster/osgcluster.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcluster/receiver.h
#%{_prefix}/share/OpenSceneGraph/src/examples/osgkeyboard/osgkeyboard.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgkeyboard/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsimplifier/osgsimplifier.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsimplifier/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgreflect/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgreflect/osgreflect.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcubemap/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcubemap/osgcubemap.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgkeyboardmouse/osgkeyboardmouse.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgkeyboardmouse/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgspacewarp/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgspacewarp/osgspacewarp.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgGLUTkeyboardmouse/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgGLUTkeyboardmouse/osgGLUTkeyboardmouse.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshaderterrain/osgshaderterrain.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshaderterrain/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgmotionblur/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgmotionblur/osgmotionblur.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgvertexprogram/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgvertexprogram/osgvertexprogram.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcamera/osgcamera.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcamera/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshaders/Noise.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshaders/GL2Scene.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshaders/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgshaders/osgshaders.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgpagedlod/osgpagedlod.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgpagedlod/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgblendequation/osgblendequation.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgblendequation/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgforest/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgforest/osgforest.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgautotransform/osgautotransform.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgautotransform/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osglogicop/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osglogicop/osglogicop.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgprecipitation/osgprecipitation.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgprecipitation/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcameragroup/osgcameragroup.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcameragroup/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtexturerectangle/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtexturerectangle/osgtexturerectangle.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghud/osghud.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghud/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgunittests/performance.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgunittests/osgunittests.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgunittests/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgscalarbar/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgscalarbar/osgscalarbar.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgspheresegment/osgspheresegment.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgspheresegment/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgspotlight/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgspotlight/osgspotlight.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgpoints/osgpoints.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgpoints/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgprerender/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgprerender/osgprerender.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgparticle/osgparticle.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgparticle/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgscribe/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgscribe/osgscribe.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsimple/osgsimple.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgsimple/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgstereoimage/osgstereoimage.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgstereoimage/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgteapot/osgteapot.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgteapot/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osggeodemo/osggeodemo.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osggeodemo/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcallback/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcallback/osgcallback.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osggeometry/osggeometry.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osggeometry/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgvolume/osgvolume.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgvolume/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgphotoalbum/osgphotoalbum.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgphotoalbum/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgphotoalbum/ImageReaderWriter.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgphotoalbum/PhotoArchive.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdistortion/osgdistortion.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdistortion/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osglightpoint/osglightpoint.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osglightpoint/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgclip/osgclip.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgclip/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcopy/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgcopy/osgcopy.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osglogo/osglogo.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osglogo/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgpick/osgpick.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgpick/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtext/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtext/osgtext.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgintrospection/osgintrospection.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgintrospection/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdepthpartition/DepthPartitionNode.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdepthpartition/osgdepthpartition.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdepthpartition/DistanceAccumulator.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdepthpartition/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtexture1D/osgtexture1D.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtexture1D/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtexture2D/osgtexture2D.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtexture2D/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtexture3D/osgtexture3D.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgtexture3D/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdelaunay/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdelaunay/osgdelaunay.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgparametric/osgparametric.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgparametric/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/terrain.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/hat.h
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/osghangglide.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/terrain_normals.h
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/sky.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/GliderManipulator.h
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/terrain_texcoords.h
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/trees.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/hat.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/GliderManipulator.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/tank.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/terrain_coords.h
#%{_prefix}/share/OpenSceneGraph/src/examples/osghangglide/base.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osganimate/osganimate.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osganimate/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgbluemarble/osgbluemarble.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgbluemarble/GNUmakefile
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdepthshadow/osgdepthshadow.cpp
#%{_prefix}/share/OpenSceneGraph/src/examples/osgdepthshadow/GNUmakefile
#
