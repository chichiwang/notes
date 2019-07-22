import unittest

from phonebook_01 import Phonebook_01 as Phonebook

class PhonebookTest_01(unittest.TestCase):

    def test_create_phonebook(self):
        phonebook = Phonebook()

    def test_lookup_entry_by_name(self):
        phonebook = Phonebook()
        phonebook.add("Bob", "12345")
        self.assertEqual("12345", phonebook.lookup("Bob"))


