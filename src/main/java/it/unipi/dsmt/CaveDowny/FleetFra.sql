DROP DATABASE IF EXISTS `FleetFra`;
CREATE DATABASE `FleetFra` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci */ /*!80016 DEFAULT ENCRYPTION='N' */;
USE `FleetFra`;
-- MySQL dump 10.13  Distrib 8.0.31, for Win64 (x86_64)
--
-- Host: 127.0.0.1    Database: FleetFra
-- ------------------------------------------------------
-- Server version	8.0.31

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `match`
--

DROP TABLE IF EXISTS `match`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `match` (
  `idMatch` int auto_increment NOT NULL,
  `User1` varchar(45) NOT NULL,
  `User2` varchar(45) NOT NULL,
  `Timestamp` datetime NOT NULL,
  `Winner` int NOT NULL

  PRIMARY KEY (`idMatch`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `match`
--

LOCK TABLES `match` WRITE;
/*!40000 ALTER TABLE `match` DISABLE KEYS */;
INSERT INTO `match` VALUES (1,'user123','jackson84','lily22',1,'2023-01-01 12:30:45'),(2,'user456','lily22','mike_jones',7,'2023-01-02 14:20:30'),(3,'jackson84','max_wilson','sophie_davis',12,'2023-01-03 16:15:22'),(4,'sophie_davis','david_moore','emma_taylor',7,'2023-01-04 18:10:15'),(5,'max_wilson','emma_taylor','alex_cook',8,'2023-01-05 20:05:12'),(6,'sarah_johnson','jackson84','owen_hall',9,'2023-01-06 22:00:00'),(7,'emma_taylor','owen_hall','ella_miller',5,'2023-01-07 23:55:45'),(8,'jackson84','sarah_johnson','liam_adams',4,'2023-01-08 01:50:30'),(9,'max_wilson','owen_hall','sarah_johnson',3,'2023-01-09 03:45:22'),(10,'owen_hall','ella_miller','luke_smith',5,'2023-01-10 05:40:15'),(11,'sarah_johnson','alex_cook','mia_brown',5,'2023-01-11 07:35:12'),(12,'liam_adams','owen_hall','grace_clark',6,'2023-01-12 09:30:00'),(13,'owen_hall','sarah_johnson','zoe_jenkins',2,'2023-01-13 11:25:45'),(14,'alex_cook','mia_brown','noah_carter',8,'2023-01-14 13:20:30'),(15,'ella_miller','luke_smith','olivia_white',9,'2023-01-15 15:15:22'),(16,'liam_adams','noah_carter','david_moore',7,'2023-01-16 17:10:15'),(17,'owen_hall','alex_cook','emma_taylor',9,'2023-01-17 19:05:12'),(18,'sarah_johnson','david_moore','max_wilson',4,'2023-01-18 21:00:00'),(19,'alex_cook','ella_miller','owen_hall',8,'2023-01-19 22:55:45'),(20,'liam_adams','zoe_jenkins','mia_brown',9,'2023-01-20 00:50:30');
/*!40000 ALTER TABLE `match` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `user` (
  `Username` varchar(45) NOT NULL,
  `Name` varchar(45) NOT NULL,
  `Surname` varchar(45) NOT NULL,
  `Password` varchar(100) NOT NULL,
  PRIMARY KEY (`Username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
INSERT INTO `user` VALUES ('alex_cook','Alex','Cook','OceanWave456'),('amy_carter','Amy','Carter','Garden789'),('david_moore','David','Moore','Moonlight789'),('ella_miller','Ella','Miller','SweetDreams123'),('emma_taylor','Emma','Taylor','Butterfly123'),('grace_clark','Grace','Clark','PurpleRain123'),('jack_robinson','Jack','Robinson','CoffeeTime456'),('jackson84','Jackson','Smith','Secure123!'),('liam_adams','Liam','Adams','SecretGarden123'),('lily22','Lily','Williams','Rainbow456'),('luke_smith','Luke','Smith','GoldenSun456'),('max_wilson','Max','Wilson','BlueSky123'),('mia_brown','Mia','Brown','HappyDay123'),('mike_jones','Mike','Jones','Summer2023!'),('noah_carter','Noah','Carter','SunnyDay789'),('olivia_white','Olivia','White','HappyPlace456'),('owen_hall','Owen','Hall','Starlight456'),('sophie_davis','Sophie','Davis','Sunflower456'),('user1','John','Doe','pass123'),('user10','Emma','Anderson','access'),('user123','Sarah','Johnson','P@ssw0rd123'),('user2','Alice','Smith','qwerty'),('user3','Bob','Johnson','secret'),('user4','Eva','Williams','mypassword'),('user5','Charlie','Brown','letmein'),('user6','Olivia','Davis','p@ssw0rd'),('user7','Michael','Miller','secure123'),('user8','Sophia','Moore','password123'),('user9','Liam','Taylor','123456'),('zoe_jenkins','Zoe','Jenkins','MountainView123');
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2023-12-14 15:02:09
