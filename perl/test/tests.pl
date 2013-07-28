# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

use Magick::Mirror;

my ($orig_top, $orig_bot, $space_top, $space_bot, $orig_mask, $top, $bottom, $mask);

$orig_bot = Magick::Mirror::new("images/bottom.png", "cached_images");
$orig_top = Magick::Mirror::new("images/top.png", "cached_images");
$orig_mask = Magick::Mirror::new("images/mask.png", "cached_images");

$space_top = Magick::Mirror::new("images/AtlantisReflection_ingalls.jpg", "cached_images");
$space_bot = Magick::Mirror::new("images/yuriearth_iss_3032.jpg", "cached_images");

$mask = $orig_mask->clone();
$bottom = $orig_bot->clone();
$top = $orig_top->clone();

$bottom->simple_transform("", '-background', 'transparent');
$bottom->extent("800x600-500+0");

$mask->simple_transform("", '-background', 'transparent');
$mask->extent("800x600-500+0");
$mask->named_checkpoint("mask-test-mask.png");

$bottom->mask($mask);

$bottom->composite($top);

$bottom->named_checkpoint("mask-test.png");
$bottom->run();

$mask = $orig_mask->clone();
$bottom = $space_bot->clone();
$top = $space_top->clone();

$top->format("png"); # gotta have an alpha channel for these next parts
$bottom->format("png"); # gotta have an alpha channel for these next parts

$bottom->resize("800!x600!");
$top->resize("800x600!");

$top->blend($bottom, $mask);

$top->named_checkpoint("blend-test.png");
$top->run();

$top = $space_top->clone();
$top->resize("800x600");
$top->format("png");
$top->shadow('black', 100, 20, '+20', '-50');

$top->named_checkpoint('shadow-test.png');

$top->run();

$top = $space_top->clone();
print $top->identify();

