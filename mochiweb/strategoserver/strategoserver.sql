-- MySQL dump 10.11
--
-- Host: localhost    Database: strategoserver
-- ------------------------------------------------------
-- Server version	5.0.83-0ubuntu3

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `game_boards`
--

/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_boards` (
  `game_board_id` varchar(100) NOT NULL default '',
  `game_id` varchar(100) default NULL,
  `game_player_id` varchar(100) default NULL,
  `x` int(11) default NULL,
  `y` int(11) default NULL,
  `r` varchar(10) default NULL,
  PRIMARY KEY  (`game_board_id`),
  KEY `game_board_idx` (`game_id`),
  KEY `game_board_idx2` (`game_player_id`),
  KEY `game_board_idx3` (`game_id`,`x`),
  KEY `game_board_idx4` (`game_id`,`y`),
  KEY `game_board_idx5` (`game_id`,`x`,`y`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `game_player_messages`
--

/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_player_messages` (
  `message_id` varchar(100) NOT NULL default '',
  `game_player_id` varchar(100) default NULL,
  `message` text,
  `time_stamp` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  PRIMARY KEY  (`message_id`),
  KEY `game_player_messages_idx` (`game_player_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `game_player_processes`
--

/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_player_processes` (
  `game_player_id` varchar(100) NOT NULL default '',
  `process_id` varchar(50) default NULL,
  PRIMARY KEY  (`game_player_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `game_players`
--

/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_players` (
  `game_player_id` varchar(100) NOT NULL default '',
  `game_id` varchar(100) default NULL,
  `is_player_1` int(11) default NULL,
  `is_board_set` varchar(10) default NULL,
  `alias` varchar(100) default NULL,
  `email_address` varchar(100) default NULL,
  PRIMARY KEY  (`game_player_id`),
  KEY `game_players_idx` (`game_id`,`is_player_1`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `games`
--

/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `games` (
  `game_id` varchar(100) NOT NULL default '',
  `game_name` varchar(100) default NULL,
  `game_status` varchar(25) default NULL,
  `game_player_id_in_turn` varchar(100) default NULL,
  `last_move` int(11) default NULL,
  `checksum` varchar(100) default NULL,
  `winning_game_player_id` varchar(100) default NULL,
  PRIMARY KEY  (`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2011-10-25 15:11:41
