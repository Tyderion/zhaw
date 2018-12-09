import unittest

def adder(x, y):
    return x + y
version = 1.0

class TestAdder(unittest.TestCase):
    """
    Test the adder function of this module
    """

    def test_add_integers(self):
        """
        Test adding integers
        """
        result = adder(1, 2)
        self.assertEqual(result, 3)

    def test_add_strings(self):
        """
        Test adding strings
        """
        result = adder("abc ", "ef")
        self.assertEqual(result, "abc ef")

    def test_add_lists(self):
        """
        Test adding lists
        """
        result = adder([1, 2], [3, 4])
        self.assertEqual(result, [1, 2, 3, 4])
    

if __name__ == '__main__':
    # Test
    unittest.main()
