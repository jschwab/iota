import utils


def test_sanitize_string_string():
    "Make sure strings get cleaned properly"

    s = "{Mart{\'{\i}}nez-Pinedo}"
    assert utils.sanitize_string(s) == "MartinezPinedo"


def test_sanitize_string_unicode():
    "Make sure unicode gets cleaned properly"

    u = u"{Mart{\'{\i}}nez-Pinedo}"
    assert utils.sanitize_string(u) == "MartinezPinedo"


def test_parse_command():
    "Make sure commands get broken down properly"

    command = 'query:"test" maxnum:10'
    results = utils.parse_command(command)
    assert results['query'] == "test"
    assert results['maxnum'] == 10


if __name__ == '__main__':
    test_parse_command()
