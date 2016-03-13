-- phpMyAdmin SQL Dump
-- version 4.1.6
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Generation Time: 27 Sep 2015 pada 11.59
-- Versi Server: 5.6.16
-- PHP Version: 5.5.9

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `abs_studikasus`
--

-- --------------------------------------------------------

--
-- Struktur dari tabel `program_i`
--

CREATE TABLE IF NOT EXISTS `program_i` (
  `idProgram` int(11) NOT NULL AUTO_INCREMENT,
  `namaProgram` varchar(255) NOT NULL,
  `departemen` varchar(255) NOT NULL,
  `tempat` varchar(255) NOT NULL,
  `tanggal` date NOT NULL,
  `peserta` int(11) NOT NULL,
  `penanggungJawab` varchar(255) NOT NULL,
  `biaya` bigint(20) NOT NULL,
  `sumberDana` varchar(255) DEFAULT NULL,
  `jenisPengeluaran` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`idProgram`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=6 ;

--
-- Dumping data untuk tabel `program_i`
--

INSERT INTO `program_i` (`idProgram`, `namaProgram`, `departemen`, `tempat`, `tanggal`, `peserta`, `penanggungJawab`, `biaya`, `sumberDana`, `jenisPengeluaran`) VALUES
(1, 'Rapat Pleno', 'Olah Raga', 'Gedung B', '2015-07-11', 100, 'kandito', 1000000, 'Fakultas', ''),
(3, 'Rapat BPH', 'BPH', 'Aula', '2015-05-11', 300, 'Bukan Saya', 100000, '', ''),
(5, 'Program 3', 'BPH', 'Depok', '2015-06-11', 200, 'Kandito', 1000000, 'Sponsor', NULL);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
