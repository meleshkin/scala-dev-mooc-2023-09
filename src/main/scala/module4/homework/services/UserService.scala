package module4.homework.services

import io.getquill.context.ZioJdbc.QIO
import module4.homework.dao.entity.{Role, RoleCode, User, UserId}
import module4.homework.dao.repository.UserRepository
import module4.phoneBook.db
import module4.phoneBook.db.DataSource
import zio.macros.accessible
import zio.{Has, RIO, ZIO, ZLayer}

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]

        def createRoleIfEmpty(role: Role): RIO[db.DataSource, Role]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx

        def listUsers(): RIO[db.DataSource, List[User]] = userRepo.list()

        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = {
            listUsers().flatMap { zioUserList =>
                ZIO.foreach(zioUserList) { user =>
                    val zioRoleList: QIO[List[Role]] = userRepo.userRoles(UserId(user.id))
                    zioRoleList.map(r => UserDTO(user, r.toSet))
                }
            }
        }

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = {
            dc.transaction {
                for {
                    _ <- userRepo.createUser(user)
                    _ <- createRoleIfEmpty(Role(roleCode.code, roleCode.code.capitalize))
                    _ <- userRepo.insertRoleToUser(roleCode, UserId(user.id))
                    roles <- userRepo.userRoles(UserId(user.id))
                } yield UserDTO(user, roles.toSet)
            }
        }
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]] = {
            userRepo.listUsersWithRole(roleCode).flatMap { zioUserList =>
                ZIO.foreach(zioUserList) { user =>
                    val zioRoleList: QIO[List[Role]] = userRepo.userRoles(UserId(user.id))
                    zioRoleList.map(r => UserDTO(user, r.toSet))
                }
            }
        }

        def createRoleIfEmpty(role: Role): RIO[DataSource, Role] = {
            for {
                maybeRole <- userRepo.findRoleByCode(RoleCode(role.code))
                result <- maybeRole.fold {userRepo.createRole(role)} {_ => ZIO.succeed(maybeRole.get)}
            } yield result
        }
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService.UserService] =
        ZLayer.fromService[UserRepository.Service, UserService.Service](userRepo =>
            new Impl(userRepo)
        )
}

case class UserDTO(user: User, roles: Set[Role])