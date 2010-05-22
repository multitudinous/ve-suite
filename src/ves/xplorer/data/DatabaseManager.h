
#pragma once

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

// --- Boost includes --- //
#include <boost/noncopyable.hpp>

// --- C++ headers --- //
#include <string>

// Forward declarations
namespace Poco
{
namespace Data
{
class Session;
class SessionPool;
}
}

namespace ves
{
namespace xplorer
{
namespace data
{

///
///@class DatabaseManager
/// Simple singleton that maintains a pool of SQLite sessions connected to
/// the application's main database. This allows an easy, centralized way to
/// manage connections to the database and to change the path of the database
/// file in a single place.
class DatabaseManager
{
public:
    ///
    /// Sets the path (including filename) of the database file.
    void SetDatabasePath( const std::string& path );

    ///
    /// Returns a pointer to the session pool. Callers can get a valid session
    /// like so:
    /// Poco::Data::Session mySession( GetPool()->get() );
    /// The session created in this way will be automatically returned to the
    /// SessionPool when mySession goes out of scope.
    Poco::Data::SessionPool* GetPool();
    
private:
    /// ctor
    DatabaseManager( );

    /// dtor
    virtual ~DatabaseManager( );

    /// Singleton declarations
    vprSingletonHeader( DatabaseManager );

    /// Holds the session pool
    Poco::Data::SessionPool* mPool;
};

}// namespace data
}// namespace xplorer
}// namespace ves
