// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include "osgBulletPlus/DataLoader.h"

#include "osgBullet/PhysicsState.h"

#include <osg/Notify>
#include <osg/NodeCallback>
#include <osg/Transform>
#include <osg/io_utils>

#include <OpenThreads/Mutex>
#include <OpenThreads/ScopedLock>

#include <string>



using namespace osgBulletPlus;


DataLoader::DataLoader()
  : _attachPoint( NULL ),
    _fileName( "" ),
    _pd( NULL ),
    _bw( NULL ),
    _state( DataLoader::INITIAL ),
    _complete( false ),
    _loadedModel( NULL ),
    _lnt( NULL ),
    _cpt( NULL )
{
    osg::notify( osg::WARN ) << "DataLoader default constructor: Why am I here?" << std::endl;
}

DataLoader::DataLoader( osg::Transform* attachPoint, const std::string& fileName, const osgBullet::PhysicsData* pd, btDynamicsWorld* bw )
  : _attachPoint( attachPoint ),
    _fileName( fileName ),
    _pd( pd ),
    _bw( bw ),
    _state( DataLoader::INITIAL ),
    _loadedModel( NULL ),
    _lnt( NULL ),
    _cpt( NULL )
{
    amt = dynamic_cast< osgTools::AbsoluteModelTransform* >( attachPoint );
    commonInit();
}

DataLoader::DataLoader( osg::Node* loadedModel, const osgBullet::PhysicsData* pd, btDynamicsWorld* bw )
  : _fileName( "" ),
    _pd( pd ),
    _bw( bw ),
    _state( DataLoader::CREATE_PHYSICS ),
    _lnt( NULL ),
    _cpt( NULL )
{
    _loadedModel = loadedModel;
    _attachPoint = loadedModel->getParent( 0 )->asTransform();
    if( _attachPoint.get() == NULL )
        osg::notify( osg::ALWAYS ) << "DataLoader: loadedModel parent is not a Transform." << std::endl;

    commonInit();
}

DataLoader::DataLoader( const DataLoader& rhs, osg::CopyOp copyop )
  : _attachPoint( rhs._attachPoint ),
    _fileName( rhs._fileName ),
    _pd( rhs._pd ),
    _bw( rhs._bw ),
    _state( rhs._state ),
    _loadedModel( rhs._loadedModel ),
    _lnt( rhs._lnt ),
    _cpt( rhs._cpt )
{
    osg::notify( osg::WARN ) << "DataLoader copy constructor: Why am I here?" << std::endl;
}

DataLoader::~DataLoader()
{
}

void
DataLoader::commonInit()
{
    _attachPoint->setDataVariance( osg::Object::DYNAMIC );

    // Error check. osgBullet only supports attach points that are
    // either MatrixTransform or AbsoluteModelTransform.
    if( ( _attachPoint->className() != std::string( "MatrixTransform" ) ) &&
        ( _attachPoint->className() != std::string( "AbsoluteModelTransform" ) ) )
        osg::notify( osg::WARN ) << "DataLoader: Attach point has incorrect class." << std::endl;

    // Error check. OSG support for nested callbacks is weak.
    if( _attachPoint->getUpdateCallback() != NULL )
        osg::notify( osg::WARN ) << "DataLoader: Attach point already has update callback." << std::endl;

    _attachPoint->setUpdateCallback( this );
}

void
DataLoader::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    if( nv->getVisitorType() != osg::NodeVisitor::UPDATE_VISITOR )
        return;


    bool finished( false );
    {
        OpenThreads::ScopedLock< OpenThreads::Mutex > _lock( _updateMutex );

        switch( _state )
        {
        case DataLoader::INITIAL:
        {
            _loadedModel = new osg::Group;
            _lnt = new LoadNodeThread( _fileName );
            _lnt->start();
            _state++;
            break;
        }
        case DataLoader::LOAD_MODEL:
        {
            if( ( _lnt != NULL ) && ( !_lnt->isRunning() ) )
            {
                if( _lnt->_node.get() != NULL )
                {
                    _loadedModel = _lnt->_node;
                    _attachPoint->addChild( _loadedModel.get() );

                    // Set the initial world trans so that the model pops up in the right place.
                    osgTools::AbsoluteModelTransform* amt = dynamic_cast< osgTools::AbsoluteModelTransform* >( _attachPoint.get() );
                    if( amt != NULL )
                    {
                        amt->setMatrix( _pd->_osgTransform );
                    }
                    else
                    {
                        osg::Transform* t = _attachPoint->asTransform();
                        if( (t!=NULL) && ( t->asMatrixTransform() != NULL ) )
                        {
                            t->asMatrixTransform()->setMatrix( _pd->_osgTransform );
                        }
                    }
                    _state++;
                }
                else
                {
                    osg::notify( osg::FATAL ) << "LoadModelThtread failed to load " << _fileName << std::endl;
                    _state = IDLE;
                }

                delete _lnt;
                _lnt = NULL;
            }
            break;
        }
        case DataLoader::CREATE_PHYSICS:
        {
            _cpt = new CreatePhysicsThread( _loadedModel.get(), _pd.get() );
            _cpt->start();
            _state++;
            break;
        }
        case DataLoader::COMPLETE:
        {
            if( ( _cpt != NULL ) && ( !_cpt->isRunning() ) )
            {
                btRigidBody* rb = _cpt->_rb;
                osgBullet::MotionState* ms = dynamic_cast< osgBullet::MotionState* >( rb->getMotionState() );
                ms->setTransform( _attachPoint->asTransform() );

                ms->setWorldTransform( osgBullet::asBtTransform( _pd->_bodyWorldTransform ) );
                rb->setWorldTransform( osgBullet::asBtTransform( _pd->_bodyWorldTransform ) );
                rb->setLinearVelocity( osgBullet::asBtVector3( _pd->_linearVelocity ) );
                rb->setAngularVelocity( osgBullet::asBtVector3( _pd->_angularVelocity ) );

                // TBD  We might collide with the Bullet physics sim
                // thread, when that project is complete.
                _bw->addRigidBody( rb );

                delete _cpt;
                _cpt = NULL;
                _state++;
            }

            break;
        }
        case DataLoader::IDLE:
        {
            finished = true;

            OpenThreads::ScopedLock< OpenThreads::Mutex > _lock( _completeMutex );
            _complete = true;

            break;
        }
        default:
            osg::notify( osg::FATAL ) << "DataLoader: Undefined state." << std::endl;
        }
    }

    traverse( node, nv );

    osg::ref_ptr< DataLoader > dl;
    if( finished )
    {
        dl = this;
        _attachPoint->setUpdateCallback( NULL );
    }

}

void
DataLoader::incrementStateSafe()
{
    OpenThreads::ScopedLock< OpenThreads::Mutex > _lock( _updateMutex );
    _state++;
}

bool
DataLoader::loadComplete() const
{
    OpenThreads::ScopedLock< OpenThreads::Mutex > _lock( _completeMutex );
    return( _complete );
}
