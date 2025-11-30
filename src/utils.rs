use std::{cell::RefCell, collections::{HashMap, HashSet}, path::{Component, Path, PathBuf}, rc::Rc};

#[derive(Debug)]
pub enum PathNodeData {
    Directory {
        name: String,
        children: Vec<Rc<RefCell<PathNode>>>,
    },
    File {
        name: String,
    },
}

impl PathNodeData {
    #[inline(always)]
    pub fn name(&self) -> &str {
        match self {
            PathNodeData::Directory { name, .. } => &name,
            PathNodeData::File { name } => name,
        }
    }
}

#[derive(Debug)]
pub struct PathNode {
    pub parent: Option<Rc<RefCell<PathNode>>>,
    pub data: PathNodeData,
}

impl PathNode {
    #[inline(always)]
    pub fn name(&self) -> &str {
        self.data.name()
    }
}

#[derive(Debug)]
pub struct PathTree {
    nodes: Vec<Rc<RefCell<PathNode>>>,
    added: HashSet<PathBuf>,
    resolved: HashMap<PathBuf, PathBuf>,
}

impl PathTree {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            added: HashSet::new(),
            resolved: HashMap::new(),
        }
    }

    pub fn add_path<P: AsRef<Path>>(&mut self, path: P, is_directory: bool) {
        let path_buffer = PathBuf::from(path.as_ref());
        let components = path.as_ref().components().collect::<Vec<_>>();
        
        if components.is_empty() || self.added.contains(&path_buffer) {
            return;
        }

        self.added.insert(path_buffer);

        let mut current_node: Option<Rc<RefCell<PathNode>>> = None;

        if components.len() > 1 {
            for component in &components[..components.len() - 1] {
                match component {
                    Component::Prefix(_) => {}
                    Component::RootDir => {}
                    Component::CurDir => {}
                    Component::ParentDir => {
                        match current_node {
                            Some(node) => {
                                current_node = node.borrow().parent.clone();
                            }
                            None => {
                                panic!("Malformed path: {}", path.as_ref().display());
                            }
                        }
                    }
                    Component::Normal(name) => {
                        let lowercase_name = name.to_string_lossy().to_lowercase();

                        // If we don't have a current node, try to find a matching directory node in the tree
                        if current_node.is_none() {
                            current_node = self.nodes.iter().find(|n| {
                                let node = n.borrow();
                                let PathNodeData::Directory { name, .. } = &node.data else {
                                    return false;
                                };
                                name.to_lowercase() == lowercase_name
                            }).cloned();

                            // If we found an existing directory in the tree, see if ours has more uppercase characters and use that instead
                            if let Some(current_node) = current_node.as_mut() {
                                // println!("found tree root directory: {:#?}", current_node.borrow().name());
                                
                                let PathNodeData::Directory { name: next_name, .. } = &mut current_node.borrow_mut().data else {
                                    unreachable!()
                                };

                                let upper1 = name.to_string_lossy().chars().filter(|c| c.is_uppercase()).collect::<Vec<_>>().len();
                                let upper2 = next_name.chars().filter(|c| c.is_uppercase()).collect::<Vec<_>>().len();

                                if upper1 > upper2 {
                                    // println!("upgrading to: {}", name.to_string_lossy());
                                    *next_name = name.to_string_lossy().to_string();
                                }
                            }
                            // If we didn't find a matching directory in the tree, add a new one to the tree, set it as the current node, then go to the next component
                            else {
                                // println!("adding tree root directory: {}", name.to_string_lossy());
                                
                                self.nodes.push(Rc::new(RefCell::new(PathNode {
                                    parent: None,
                                    data: PathNodeData::Directory {
                                        name: name.to_string_lossy().to_string(),
                                        children: vec![],
                                    },
                                })));

                                current_node = self.nodes.last().cloned();
                            }

                            continue;
                        }

                        // Try to find an existing directory in the current directory
                        let node = current_node.clone().unwrap();
                        let mut node = node.borrow_mut();
                        // let node_name = node.name().to_string();

                        let PathNodeData::Directory { children, .. } = &mut node.data else {
                            unreachable!()
                        };

                        let mut next_node = children.iter().find(|n| {
                            let PathNodeData::Directory { name, .. } = &n.borrow().data else {
                                return false;
                            };
                            name.to_lowercase() == lowercase_name
                        }).cloned();

                        // If we found an existing directory under the current directory, see if ours has more uppercase characters and use that instead
                        if let Some(next_node) = next_node.as_mut() {
                            let PathNodeData::Directory { name: next_name, .. } = &mut next_node.borrow_mut().data else {
                                unreachable!()
                            };

                            // println!("found inner directory: {}", next_name);
                            
                            let upper1 = name.to_string_lossy().chars().filter(|c| c.is_uppercase()).collect::<Vec<_>>().len();
                            let upper2 = next_name.chars().filter(|c| c.is_uppercase()).collect::<Vec<_>>().len();

                            if upper1 > upper2 {
                                // println!("upgrading to: {}", name.to_string_lossy());
                                *next_name = name.to_string_lossy().to_string();
                            }
                        }
                        // If we didn't find an existing directory under the current directory, add a new directory and set it as the next node
                        else {
                            let new_node = Rc::new(RefCell::new(PathNode {
                                parent: current_node.clone(),
                                data: PathNodeData::Directory {
                                    name: name.to_string_lossy().to_string(),
                                    children: vec![],
                                },
                            }));

                            // println!("adding \"{}\" to \"{}\"", name.to_string_lossy(), node_name);

                            children.push(new_node.clone());

                            next_node = Some(new_node);
                        }

                        current_node = next_node;
                    }
                }
            }
        }
        
        let Component::Normal(name) = components.last().unwrap() else {
            panic!("Malformed path: {}", path.as_ref().display());
        };

        let parent_node = current_node.clone();

        if let Some(node) = current_node.as_mut() {
            let PathNodeData::Directory { children, .. } = &mut node.borrow_mut().data else {
                unreachable!()
            };

            let lowercase_name = name.to_string_lossy().to_lowercase();

            let mut next_node = children.iter().find(|n| {
                let node = n.borrow();

                let name = if is_directory {
                    let PathNodeData::Directory { name, .. } = &node.data else {
                        // println!("Expected directory, found {:?}", node.data);
                        return false;
                    };
                    name
                } else {
                    let PathNodeData::File { name } = &node.data else {
                        // let PathNodeData::Directory { name, .. } = &node.data else {
                        //     unreachable!()
                        // };
                        // println!("Expected file, found directory {}", name);
                        return false;
                    };
                    name
                };

                name.to_lowercase() == lowercase_name
            }).cloned();

            // If we found an existing node under the current directory, see if ours has more uppercase characters and use that instead
            if let Some(next_node) = next_node.as_mut() {
                let mut next_node = next_node.borrow_mut();

                let next_name = match &mut next_node.data {
                    PathNodeData::Directory { name, .. } if is_directory => name,
                    PathNodeData::File { name } if !is_directory => name,
                    _ => unreachable!(),
                };

                // println!("found final directory: {}", next_name);
                
                let upper1 = name.to_string_lossy().chars().filter(|c| c.is_uppercase()).collect::<Vec<_>>().len();
                let upper2 = next_name.chars().filter(|c| c.is_uppercase()).collect::<Vec<_>>().len();

                if upper1 > upper2 {
                    // println!("upgrading to: {}", name.to_string_lossy());
                    *next_name = name.to_string_lossy().to_string();
                }
            }
            // If we didn't find an existing node under the current directory, add a new node
            else {
                // println!("BLOOP: {}", name.to_string_lossy());
                children.push(Rc::new(RefCell::new(PathNode {
                    parent: parent_node,
                    data: if is_directory {
                        PathNodeData::Directory {
                            name: name.to_string_lossy().to_string(),
                            children: vec![],
                        }
                    } else {
                        PathNodeData::File {
                            name: name.to_string_lossy().to_string(),
                        }
                    },
                })));
            }
        } else {
            self.nodes.push(Rc::new(RefCell::new(PathNode {
                parent: None,
                data: if is_directory {
                    PathNodeData::Directory {
                        name: name.to_string_lossy().to_string(),
                        children: vec![],
                    }
                } else {
                    PathNodeData::File {
                        name: name.to_string_lossy().to_string(),
                    }
                },
            })));
        }
    }

    pub fn resolve_path<P: AsRef<Path>>(&mut self, path: P, is_directory: bool) -> Option<PathBuf> {
        let path_buffer = PathBuf::from(path.as_ref());
        let components = path.as_ref().components().collect::<Vec<_>>();

        if components.is_empty() {
            return Some(path_buffer);
        }

        if let Some(result) = self.resolved.get(&path_buffer) {
            return Some(result.clone());
        }

        let mut result = PathBuf::new();
        result.push(Component::RootDir);
        
        let mut current_node: Option<Rc<RefCell<PathNode>>> = None;

        if components.len() > 1 {
            for component in &components[..components.len() - 1] {
                match component {
                    Component::Prefix(_) => {}
                    Component::RootDir => {
                        result.push(component);
                    }
                    Component::CurDir => {}
                    Component::ParentDir => {
                        match current_node.clone() {
                            Some(node) => {
                                current_node = node.borrow().parent.clone();
                                result.pop();
                            }
                            None => {
                                panic!("Malformed path: {}", path.as_ref().display());
                            }
                        }
                    }
                    Component::Normal(name) => {
                        let lowercase_name = name.to_string_lossy().to_lowercase();

                        // If we don't have a current node, try to find a matching directory node in the tree
                        if current_node.is_none() {
                            current_node = self.nodes.iter().find(|n| {
                                let node = n.borrow();
                                let PathNodeData::Directory { name, .. } = &node.data else {
                                    return false;
                                };
                                name.to_lowercase() == lowercase_name
                            }).cloned();

                            let Some(node) = current_node.clone() else {
                                // println!("1");
                                return None;
                            };

                            result.push(node.borrow().name());
                            continue;
                        }

                        // Try to find an existing directory in the current directory
                        let node = current_node.clone().unwrap();
                        let node = node.borrow();

                        let PathNodeData::Directory { children, .. } = &node.data else {
                            unreachable!()
                        };

                        let next_node = children.iter().find(|n| {
                            let node = n.borrow();
                            let PathNodeData::Directory { name, .. } = &node.data else {
                                return false;
                            };
                            name.to_lowercase() == lowercase_name
                        }).cloned();

                        let Some(node) = next_node.as_ref() else {
                            // println!("2");
                            return None;
                        };

                        result.push(node.borrow().name());
                        current_node = next_node;
                    }
                }
            }
        }
        
        let Component::Normal(name) = components.last().unwrap() else {
            panic!("Malformed path: {}", path.as_ref().display());
        };

        let Some(node) = current_node.as_mut() else {
            return None;
        };

        let PathNodeData::Directory { children, .. } = &mut node.borrow_mut().data else {
            unreachable!()
        };

        let lowercase_name = name.to_string_lossy().to_lowercase();

        let next_node = children.iter().find(|n| {
            let node = n.borrow();

            let name = if is_directory {
                let PathNodeData::Directory { name, .. } = &node.data else {
                    return false;
                };
                name
            } else {
                let PathNodeData::File { name } = &node.data else {
                    return false;
                };
                name
            };

            name.to_lowercase() == lowercase_name
        }).cloned();

        let Some(node) = next_node.as_ref() else {
            // println!("3");
            return None;
        };

        result.push(node.borrow().name());

        self.resolved.insert(path_buffer, result.clone());
        
        Some(result)
    }
}

#[inline(always)]
pub fn sanitize_path<S: AsRef<str>>(path: S) -> String {
    let mut path = path.as_ref().to_string().replace('\\', "/");

    if let Some((_, rest)) = path.split_once(':') {
        path = rest.to_string();
    }

    while path.contains("//") {
        path = path.replace("//", "/");
    }

    let path = PathBuf::from(path.trim_start_matches("\\\\?\\"));

    let mut components = vec![];

    for component in path.components() {
        match component {
            Component::CurDir => {}
            
            Component::ParentDir => {
                components.pop();
            }

            _ => components.push(component),
        }
    }

    let mut result = PathBuf::new();
    result.extend(components);

    result.to_string_lossy().to_string()
}

#[inline(always)]
pub fn canonicalize_path(root_path: &str, path: &str, is_directory: bool) -> PathBuf {
    let mut path = format!(
        "{}{}",
        sanitize_path(if path.contains(':') {
            format!("{}", path)
        } else {
            format!("{}/{}", root_path, path)
        }),
        if is_directory { "/" } else { "" },
    );

    while path.contains("//") {
        path = path.replace("//", "/");
    }

    path.into()
}
